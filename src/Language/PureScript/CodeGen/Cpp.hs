-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code in the simplified C++11 intermediate representation from Purescript code
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp (
    module AST,
    module Common,
    moduleToCpp
) where

import Data.List
import Data.Char
import Data.Function (on)
import Data.Maybe
import qualified Data.Traversable as T (traverse)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Language.PureScript.AST.Declarations (ForeignCode(..))
import Language.PureScript.CodeGen.Cpp.AST as AST
import Language.PureScript.CodeGen.Cpp.Common as Common
import Language.PureScript.CodeGen.Cpp.Optimizer
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Core
import Language.PureScript.CoreImp.Operators
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import Language.PureScript.Environment
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.CoreImp.AST as CI
import qualified Language.PureScript.Types as T
import qualified Language.PureScript.TypeClassDictionaries as TCD
import qualified Language.PureScript.Pretty.Cpp as P

import Debug.Trace

data DeclLevel = TopLevel | InnerLevel deriving (Eq, Show);

-- |
-- Generate code in the simplified C++11 intermediate representation for all declarations in a
-- module.
--
moduleToCpp :: forall m mode. (Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => Environment -> Module (CI.Decl Ann) ForeignCode -> m [Cpp]
moduleToCpp env (Module coms mn imps exps foreigns decls) = do
  additional <- asks optionsAdditional
  cppImports <- T.traverse (pure . runModuleName) . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ imps
  let cppImports' = "PureScript" : cppImports
  let foreigns' = [] -- mapMaybe (\(_, cpp, _) -> CppRaw . runForeignCode <$> cpp) foreigns
  cppDecls <- mapM (declToCpp TopLevel) decls
  optimized <- T.traverse optimize (concatMap expandSeq cppDecls)
  let optimized' = removeCodeAfterReturnStatements <$> optimized
  let isModuleEmpty = null exps
  comments <- not <$> asks optionsNoComments
  datas <- modDatasToCpps
  let moduleHeader = (CppInclude <$> cppImports')
                  ++ P.linebreak
                  ++ [CppNamespace (runModuleName mn) $
                       (CppUseNamespace <$> cppImports') ++ P.linebreak ++ datas ++ toHeader optimized']
  let bodyCpps = toBody optimized'
      moduleBody = CppInclude (runModuleName mn) : P.linebreak
                ++ (if null bodyCpps
                      then []
                      else [CppNamespace (runModuleName mn) $
                             (CppUseNamespace <$> cppImports') ++ P.linebreak ++ bodyCpps])
                ++ (if isMain mn then [nativeMain] else [])
  return $ case additional of
    MakeOptions -> moduleHeader ++ CppEndOfHeader : moduleBody
    CompileOptions _ _ _ | not isModuleEmpty -> moduleBody
    _ -> []

  where
  -- TODO: remove this after CoreImp optimizations are finished
  --
  removeCodeAfterReturnStatements :: Cpp -> Cpp
  removeCodeAfterReturnStatements = everywhereOnCpp (removeFromBlock go)
    where
    go :: [Cpp] -> [Cpp]
    go cpps | not (any isCppReturn cpps) = cpps
            | otherwise = let (body, ret : _) = span (not . isCppReturn) cpps in body ++ [ret]
    isCppReturn (CppReturn _) = True
    isCppReturn _ = False
    removeFromBlock :: ([Cpp] -> [Cpp]) -> Cpp -> Cpp
    removeFromBlock go' (CppBlock sts) = CppBlock (go' sts)
    removeFromBlock _  cpp = cpp

  declToCpp :: DeclLevel -> CI.Decl Ann -> m Cpp
  -- |
  -- Typeclass instance definition
  --
  declToCpp _ (CI.VarDecl _ ident expr)
    | Just (classname@(Qualified _ (ProperName unqualClass)), typs) <- findInstance (Qualified (Just mn) ident),
      Just (_, _, fns) <- findClass classname = do
    let (_, fs) = unApp expr []
        fs' = filter (isNormalFn) fs
    cpps <- mapM toCpp (zip fns fs')
    return $ CppStruct (unqualClass, Right (catMaybes typs)) [] cpps []
    where
    toCpp :: ((String, T.Type), CI.Expr Ann) -> m Cpp
    toCpp ((name, _), CI.AnonFunction ty ags sts) = do
      fn' <- declToCpp TopLevel $ CI.Function ty (Ident name) ags sts
      return (addQual CppStatic fn')
    toCpp ((name, _), e@CI.Literal{})
      | Just ty <- tyFromExpr e = declToCpp InnerLevel (CI.VarDecl (Nothing, [], Just ty, Nothing) (Ident name) e)
    toCpp ((name, _), e) -- Note: for vars, avoiding templated args - a C++14 feature - for now
      | Just ty <- tyFromExpr e,
        tparams <- templparams' (mktype mn ty) = varDeclToFn name e ty tparams [CppInline, CppStatic]
    toCpp ((name, _), e)
      | Just ty <- tyFromExpr e = declToCpp InnerLevel (CI.VarDecl (Nothing, [], Just ty, Nothing) (Ident name) e)
    toCpp ((name, _), e) = return $ trace (name ++ " :: " ++ show e ++ "\n") CppNoOp

    addQual :: CppQualifier -> Cpp -> Cpp
    addQual q (CppFunction name tmps args rty qs cpp) = CppFunction name tmps args rty (q : qs) cpp
    addQual _ cpp = cpp

    isNormalFn :: (CI.Expr Ann) -> Bool
    isNormalFn (CI.AnonFunction _
                 [Ident "__unused"]
                 [CI.Return (_, _, Nothing, Nothing) (CI.Var (_, _, Nothing, Nothing) _)]) = False
    isNormalFn _ = True

  -- Note: for vars, avoiding templated args - a C++14 feature - for now
  declToCpp TopLevel (CI.VarDecl _ (Ident name) expr)
    | Just ty <- tyFromExpr expr,
      ty'@(Just Function{}) <- mktype mn ty,
      tparams@(_:_) <- templparams' ty' = varDeclToFn name expr ty tparams [CppInline]

  declToCpp _ (CI.VarDecl (_, _, ty, _) ident expr) = do
    expr' <- exprToCpp expr
    return $ CppVariableIntroduction (identToCpp ident, ty >>= mktype mn) [] (Just expr')

  -- TODO: fix when proper Meta info added
  declToCpp TopLevel (CI.Function _ _ [Ident "dict"]
    [CI.Return _ (CI.Accessor _ _ (CI.Var _ (Qualified Nothing (Ident "dict"))))]) =
    return CppNoOp

  declToCpp lvl (CI.Function (ss, com, Just (T.ConstrainedType ts ty), _) ident _ [body]) = do
    let fn' = drop' (length ts) ([], body)
    declToCpp lvl $ CI.Function (ss, com, Just ty, Nothing) ident (fst fn') [snd fn']
      where
        drop' :: Int -> ([Ident], CI.Statement Ann) -> ([Ident], CI.Statement Ann)
        drop' n (args, CI.Return _ (CI.AnonFunction _ args' [body'])) | n > 0 = drop' (n-1) (args ++ args', body')
        drop' _ a = a

  declToCpp TopLevel (CI.Function (_, _, Just ty, _) ident [arg] body) = do
    block <- CppBlock <$> mapM statmentToCpp body
    let typ = mktype mn ty
    return $ CppFunction (identToCpp ident)
                         (templparams' typ)
                         [(identToCpp arg, argtype typ)]
                         (rettype typ)
                         []
                         block

  -- C++ doesn't support nested functions, so use lambdas
  declToCpp InnerLevel (CI.Function (_, _, Just ty, _) ident [arg] body) = do
    block <- CppBlock <$> mapM statmentToCpp body
    return $ CppVariableIntroduction (identToCpp ident, Nothing) [] (Just (asLambda block))
    where
    asLambda block' =
      let typ = mktype mn ty
          tmps = maybe [] (map Just . templateVars) typ
          argName = identToCpp arg
          argType = argtype typ
          retType = rettype typ
          replacements = replacementPair (argName, argType) <$> tmps
          replacedBlock = replaceTypes replacements block'
          removedTypes = zip tmps (repeat Nothing)
          replacedArgs = applyChanges removedTypes argType
          replacedRet = applyChanges removedTypes retType
      in CppLambda [(argName, replacedArgs)] replacedRet replacedBlock
      where
      replacementPair :: (String, Maybe Type) -> Maybe Type -> (Maybe Type, Maybe Type)
      replacementPair (argName, argType) typ'
        | argType == typ' = (argType, Just (DeclType argName))
      replacementPair (_, argType) _ = (argType, Nothing)

      replaceTypes :: [(Maybe Type, Maybe Type)] -> Cpp -> Cpp
      replaceTypes [] = id
      replaceTypes typs = everywhereOnCpp replace
        where
        replace (CppLambda [(argn, argt)] rett b) = CppLambda [(argn, applyChanges typs argt)] (applyChanges typs rett) b
        replace (CppInstance mn' cls iname ps) = CppInstance mn' cls iname (zip (fst <$> ps) (applyChanges typs . snd <$> ps))
        replace (CppAccessor typ p e) = CppAccessor (applyChanges typs typ) p e
        replace other = other

      applyChanges :: [(Maybe Type, Maybe Type)] -> Maybe Type -> Maybe Type
      applyChanges [] t = t
      applyChanges _ Nothing  = Nothing
      applyChanges ts t = go t
        where
        go :: Maybe Type -> Maybe Type
        go tmp@(Just (Template _ _)) | Just t' <- lookup tmp ts = t'
        go (Just (Function t1 t2)) | isNothing (go (Just t1)) || isNothing (go (Just t2)) = Nothing
        go (Just (Function t1 t2)) | Just t1' <- go (Just t1),
                                     Just t2' <- go (Just t2) = Just (Function t1' t2')
        go (Just (Native _ t2s)) | any isNothing (go . Just <$> t2s) = Nothing
        go (Just (Native t1 t2s)) = let t2s' = catMaybes $ go . Just <$> t2s in Just (Native t1 t2s')
        go (Just (EffectFunction t1)) | Just t' <- lookup (Just t1) ts = t'
        go t' = t'

  -- This covers 'let' expressions
  declToCpp InnerLevel (CI.Function (_, _, Nothing, Nothing) ident [arg] body) = do
    block <- CppBlock <$> mapM statmentToCpp body
    return $ CppVariableIntroduction (identToCpp ident, Nothing) []
               (Just (CppLambda [(identToCpp arg, Nothing)] Nothing block))

  declToCpp InnerLevel CI.Function{} = return CppNoOp -- TODO: Are these possible?
  declToCpp _ (CI.Function (_, _, Just _, _) _ _ _) = return CppNoOp -- TODO: support non-curried functions?

  -- |
  -- Typeclass declaration
  --
  declToCpp TopLevel (CI.Constructor (_, _, _, Just IsTypeClassConstructor) _ (Ident ctor) _)
    | Just (params, constraints, fns) <- findClass (Qualified (Just mn) (ProperName ctor)) =
    let tmps = runType . flip Template [] <$> params
        fnTemplPs = nub $ (concatMap (templparams' . mktype mn . snd) fns) ++
                          (concatMap constraintParams constraints)
        classTemplParams = zip tmps $ fromMaybe 0 . flip lookup fnTemplPs <$> tmps
    in
    return $ CppStruct (ctor, Left classTemplParams)
                       (toStrings <$> constraints)
                       (toCpp tmps <$> fns)
                       []
    where
    tmpParams :: [(String, T.Type)] -> [(String, Int)]
    tmpParams fns = concatMap (templparams' . mktype mn . snd) fns
    constraintParams :: T.Constraint -> [(String, Int)]
    constraintParams (cname@(Qualified _ pn), cps)
      | Just (ps, _, fs) <- findClass cname,
        ps' <- runType . flip Template [] <$> ps,
        tps@(_:_) <- filter ((`elem` ps') . fst) (tmpParams fs),
        ts@(_:_) <- (\p -> case find ((== p) . fst) tps of
                             Just (p', n) -> n
                             _ -> 0) <$> ps' = let cps' = typestr mn <$> cps in
                                               zip cps' ts
    constraintParams _ = []

    toCpp :: [String] -> (String, T.Type) -> Cpp
    toCpp tmps (name, ty)
      | ty'@(Just _) <- mktype mn ty,
        Just atyp <- argtype ty',
        Just rtyp <- rettype ty'
      = CppFunction name
                    (filter ((`notElem` tmps) . fst) $ templparams' ty')
                    [([], Just atyp)]
                    (Just rtyp)
                    [CppStatic]
                    CppNoOp
    toCpp tmps (name, ty) =
      -- TODO: need better handling for these. Variable templates would help.
      let typ = mktype mn ty
          qs | Just typ' <- typ,
               tmps'@(_:_) <- templateVars typ',
               any (`notElem` tmps) (runType <$> tmps') = [CppIgnored]
             | otherwise = [] in
      CppVariableIntroduction (name, typ) (qs ++ [CppStatic]) Nothing
    toStrings :: (Qualified ProperName, [T.Type]) -> (String, [String])
    toStrings (name, tys) = (qualifiedToStr' (Ident . runProperName) name, typestr mn <$> tys)

  -- |
  -- data declarations (to omit)
  --
  declToCpp _ (CI.Constructor (_, _, _, Just IsNewtype) _ _ _) = return CppNoOp
  declToCpp _ (CI.Constructor _ _ _ []) = return CppNoOp
  declToCpp _ (CI.Constructor (_, _, _, _) _ _ _) = return CppNoOp

  declToCpp TopLevel _ = return CppNoOp -- TODO: includes Function IsNewtype

  statmentToCpp :: CI.Statement Ann -> m Cpp
  statmentToCpp (CI.Expr e) = exprToCpp e
  statmentToCpp (CI.Decl d) = declToCpp InnerLevel d
  statmentToCpp (CI.Assignment _ assignee expr) =
    CppAssignment <$> exprToCpp assignee <*> exprToCpp expr
  statmentToCpp (CI.Loop _ cond body) =
    CppWhile <$> exprToCpp cond <*> (CppBlock <$> mapM loopStatementToCpp body)
  statmentToCpp (CI.IfElse _ cond thens (Just elses)) = do
    thens' <- CppBlock <$> mapM statmentToCpp thens
    elses' <- CppBlock <$> mapM statmentToCpp elses
    CppIfElse <$> exprToCpp cond <*> pure thens' <*> pure (Just elses')
  statmentToCpp (CI.IfElse _ cond thens Nothing) = do
    block <- CppBlock <$> mapM statmentToCpp thens
    thens' <- addTypes cond block
    CppIfElse <$> exprToCpp cond <*> pure thens' <*> pure Nothing
    where
    addTypes :: CI.Expr Ann -> Cpp -> m Cpp
    addTypes (CI.IsTagOf _ ctor e) sts = do
      e' <- exprToCpp e
      return (typeAccessors e' sts)
      where
      typeAccessors :: Cpp -> Cpp -> Cpp
      typeAccessors acc = everywhereOnCpp convert
        where
        convert :: Cpp -> Cpp
        convert (CppAccessor Nothing prop cpp) | cpp == acc = CppAccessor qtyp prop cpp
        convert cpp = cpp
        qtyp :: Maybe Type
        qtyp = Just (Native (qualifiedToStr' (Ident . runProperName) ctor) [])
    addTypes _ cpp = return cpp
  statmentToCpp (CI.Return _ expr) =
    CppReturn <$> exprToCpp expr
  statmentToCpp (CI.Throw _ msg) =
    return . CppThrow $ CppApp (CppVar "runtime_error") [CppStringLiteral msg]
  statmentToCpp (CI.Label _ lbl stmnt) =
    CppLabel lbl <$> statmentToCpp stmnt
  statmentToCpp (CI.Comment _ coms') =
    return $ CppComment coms' (CppBlock []) -- whoops

  loopStatementToCpp :: CI.LoopStatement Ann -> m Cpp
  loopStatementToCpp (CI.Break _ lbl) = return . CppBreak $ fromMaybe "" lbl
  loopStatementToCpp (CI.Continue _ lbl) = return . CppContinue $ fromMaybe "" lbl
  loopStatementToCpp (CI.Statement s) = statmentToCpp s

  exprToCpp :: CI.Expr Ann -> m Cpp
  exprToCpp (CI.Literal _ lit) =
    literalToValueCpp lit
  exprToCpp (CI.Accessor _ prop@(CI.Literal _ (StringLiteral name)) expr@(CI.Var (_, _, ty, _) _))
    | not ("__superclass_" `isPrefixOf` name) = do -- TODO: fix when proper Meta info added
    expr' <- exprToCpp expr
    prop' <- exprToCpp prop
    return (toCpp expr' prop')
    where
    toCpp :: Cpp -> Cpp -> Cpp
    toCpp e p | Just ty' <- ty,
                Just (Map pairs) <- mktype mn ty',
                Just t <- lookup name pairs = CppMapAccessor (CppCast CppNoOp t) (CppIndexer p e)
    toCpp e _ = CppAccessor (ty >>= mktype mn) name e
  exprToCpp (CI.Accessor _ prop expr) =
    CppIndexer <$> exprToCpp prop <*> exprToCpp expr
  exprToCpp (CI.Indexer _ index expr) =
    CppIndexer <$> exprToCpp index <*> exprToCpp expr
  exprToCpp (CI.AnonFunction (_, _, Just ty, _) [arg] stmnts') = do
    body <- CppBlock <$> mapM statmentToCpp stmnts'
    let typ = mktype mn ty
    return $ CppLambda [(identToCpp arg, argtype typ)] (rettype typ) body
  exprToCpp (CI.AnonFunction _ _ _) = return CppNoOp -- TODO: non-curried lambdas

  -- |
  -- Function application
  --
  exprToCpp (CI.App _ f []) = flip CppApp [] <$> exprToCpp f
  exprToCpp e@CI.App{} = do
    let (f, args) = unApp e []
    args' <- mapM exprToCpp args
    case f of
      CI.Var (_, _, _, Just IsNewtype) _ -> return (head args')

      CI.Var (_, _, Just ty, Just (IsConstructor _ fields)) name ->
        let fieldCount = length fields
            argsNotApp = fieldCount - length args
            qname = qualifiedToStr' id name
            tmps = maybe [] getDataTypeArgs (mktype mn ty >>= getDataType qname)
            val = CppDataConstructor qname tmps in
        if argsNotApp > 0
          then let argTypes = maybe [] (init . fnTypesN fieldCount) (mktype mn ty) in
               return (CppPartialApp val args' argTypes argsNotApp)
          else return (CppApp val args')

      CI.Var (_, _, _, Just IsTypeClassConstructor) _ ->
        return CppNoOp
      CI.Var (_, _, Just ty, _) (Qualified (Just mn') ident) -> do
        let (targs, argsToApply) = if length normalArgs == length args
                                     then (templArgs, args')
                                     else (templArgsFromDicts, filteredArgs)
        fnToApply <- asTemplate (snd <$> targs) <$> exprToCpp f
        return $ flip (foldl (\fn' a -> CppApp fn' [a])) argsToApply fnToApply
        where
        normalArgs :: [CI.Expr Ann]
        normalArgs = filter (not . isDict) args

        filteredArgs :: [Cpp]
        filteredArgs = catMaybes $ fst <$> templsFromDicts

        templArgsFromDicts :: [(Type, Type)]
        templArgsFromDicts = sortBy (compare `on` runType . fst) . nub $ concatMap snd templsFromDicts

        templArgs :: [(Type, Type)]
        templArgs = sortBy (compare `on` runType . fst) . nub . concat $ templateArgs <$> tysMapping
          where
          fnTyList = maybe [] (fnTypesN (length normalArgs)) (mktype mn ty)
          exprTyList = (tyFromExpr' <$> normalArgs) ++ [tyFromExpr' e]
          tysMapping = (\(a,b) -> (a, fromJust b)) <$> filter (isJust . snd) (zip fnTyList exprTyList)

        templsFromDicts :: [(Maybe Cpp, [(Type, Type)])]
        templsFromDicts = go <$> args'
          where
          go :: Cpp -> (Maybe Cpp, [(Type, Type)])
          go (CppInstance mname (cn, fns) inst params)
            | runModuleName mn' == mname,
              identToCpp ident `elem` fns
              = let paramNames = fst <$> params
                    params' = if any (isNothing . snd) params
                                then let ps = (flip lookup templArgs . flip Template [] . fst) <$> params
                                     in zip paramNames ps
                                else params
                    arg' = CppInstance mname (cn, fns) inst params'
                in (Just arg', filter ((`notElem` paramNames) . runType . fst) templArgs)
            | otherwise = (Nothing, templArgs)
          go (CppApp (CppIndexer _ inst@CppInstance{}) [CppVar "undefined"]) = go inst
          go arg = (Just arg, [])

      -- TODO: verify this
      _ -> flip (foldl (\fn a -> CppApp fn [a])) args' <$> exprToCpp f

  exprToCpp (CI.Var (_, _, ty, Just (IsConstructor _ fields)) ident) =
    let qname = qualifiedToStr' id ident
        tmps = maybe [] getDataTypeArgs (ty >>= mktype mn >>= getDataType qname)
        fieldCount = length fields in
    return $ if fieldCount == 0
               then CppApp (CppDataConstructor (qualifiedToStr' id ident) tmps) []
               else let argTypes = maybe [] (init . fnTypesN fieldCount) (ty >>= mktype mn) in
                    CppPartialApp (CppDataConstructor (qualifiedToStr' id ident) tmps) [] argTypes fieldCount

  -- Typeclass instance dictionary
  exprToCpp (CI.Var (_, _, Nothing, Nothing) ident@(Qualified (Just _) (Ident instname)))
    | Just (qname@(Qualified (Just mn') (ProperName cname)), types') <- findInstance ident,
      Just (params, _, fns) <- findClass qname
    = let fs' = fst <$> fns
          params' = runType . flip Template [] <$> params
      in return $ CppInstance (runModuleName mn') (cname, fs') instname (zip params' types')
 -- Constraint typeclass dictionary
 -- TODO: Make sure this pattern will not change in PS
  exprToCpp (CI.Var (_, _, Nothing, Nothing) (Qualified Nothing (Ident name)))
    | Just prefixStripped <- stripPrefix "__dict_"  name,
      '_' : reversedName <- dropWhile isNumber (reverse prefixStripped),
      cname <- reverse $ takeWhile (not . isPunctuation) reversedName,
      mname <- reverse . drop 1 $ dropWhile (not . isPunctuation) reversedName,
      Just (params, supers, fns) <- findClass (Qualified (Just (ModuleName [ProperName mname])) (ProperName cname))
    = let superFns = getFns supers
          fs' = fst <$> fns ++ superFns
      in return $ CppInstance mname (cname, fs') [] (zip params (repeat Nothing))
    where
    getFns :: [T.Constraint] -> [(String, T.Type)]
    getFns = concatMap go
      where
      go :: T.Constraint -> [(String, T.Type)]
      go cls | Just (_, clss, fns) <- findClass (fst cls) = fns ++ getFns clss
      go _ = []

  exprToCpp (CI.Var _ ident) =
    return $ varToCpp ident
    where
    varToCpp :: Qualified Ident -> Cpp
    varToCpp (Qualified Nothing ident') = CppVar (identToCpp ident')
    varToCpp qual = qualifiedToCpp id qual
  exprToCpp (CI.ObjectUpdate _ obj ps) = do
    obj' <- exprToCpp obj
    ps' <- mapM (sndM exprToCpp) ps
    extendObj obj' ps'
  exprToCpp (CI.UnaryOp _ op expr) =
    CppUnary (unaryToCpp op) <$> exprToCpp expr
  exprToCpp (CI.BinaryOp _ op lhs rhs) =
    CppBinary op <$> exprToCpp lhs <*> exprToCpp rhs
  exprToCpp (CI.IsTagOf (_, _, ty, _) ctor expr) =
    let qname = qualifiedToStr' (Ident . runProperName) ctor
        tmps = maybe [] getDataTypeArgs (ty >>= mktype mn >>= getDataType qname)
        val = CppData qname tmps in
    flip CppInstanceOf val <$> exprToCpp expr

  modDatasToCpps :: m [Cpp]
  modDatasToCpps
    | ds@(_:_) <- M.toList
                 . M.mapWithKey (\_ a -> snd a)
                 . M.filterWithKey (\(Qualified mn' _) _ -> mn' == Just mn)
                 . M.filter (isData . snd)
                 $ types env = do
      let types' = dataTypes ds
          (ctors, aliases) = dataCtors ds
      return (types' ++ asManaged types' ++ ctors ++ asManaged ctors ++ aliases)
    | otherwise = return []
      where
      isData :: TypeKind -> Bool
      isData DataType{} = True
      isData _ = False

      asManaged:: [Cpp] -> [Cpp]
      asManaged = concatMap go
        where
        go :: Cpp -> [Cpp]
        go (CppNamespace ns cpps) = fromStruct ns <$> cpps
        go _ = []
        fromStruct :: String -> Cpp -> Cpp
        fromStruct ns (CppStruct (name, Left tmps) _ _ _) =
          CppTypeAlias (name, tmps) (ns ++ "::" ++ name, tmps) "managed"
        fromStruct _ _ = CppNoOp

      dataTypes :: [(Qualified ProperName, TypeKind)] -> [Cpp]
      dataTypes ds | cpps@(_:_) <- concatMap go ds = [CppNamespace "type" cpps]
                   | otherwise = []
        where
        go :: (Qualified ProperName, TypeKind) -> [Cpp]
        go (_, DataType _ [_]) = []
        go (typ, DataType ts _) =
          [CppStruct (qual typ, Left (flip (,) 0 . runType . flip Template [] . fst <$> ts))
                     [] []
                     [CppFunction (qual typ) [] [] Nothing [CppVirtual, CppDestructor, CppDefault] CppNoOp]]
        go _ = []

      dataCtors :: [(Qualified ProperName, TypeKind)] -> ([Cpp], [Cpp])
      dataCtors ds | cpps@(_:_) <- concatMap (fst . go) ds,
                     aliases <- catMaybes $ map (snd . go) ds = ([CppNamespace "value" cpps], aliases)
                   | otherwise = ([],[])
        where
        go :: (Qualified ProperName, TypeKind) -> ([Cpp], Maybe Cpp)
        go (typ, DataType ts cs) = (map ctorStruct cs, alias)
          where
          alias :: Maybe Cpp
          alias | [_] <- cs =
            Just $ CppTypeAlias (qual typ, tmps)
                                (P.prettyPrintCpp [flip CppData [] . runProperName . fst $ head cs], tmps)
                                []
                | otherwise = Nothing
          tmps :: [(String, Int)]
          tmps = flip (,) 0 . runType . flip Template [] . fst <$> ts
          ctorStruct :: (ProperName, [T.Type]) -> Cpp
          ctorStruct (ctor, fields) =
            CppStruct (name, Left tmps) supers [] members
            where
            name :: String
            name = P.prettyPrintCpp [flip CppData [] $ runProperName ctor]
            supers :: [(String, [String])]
            supers | [_] <- cs = []
                   | otherwise = [(addNamespace "type" (qual typ), fst <$> tmps)]
            members :: [Cpp]
            members | fields'@(_:_) <- filter (/=(Just (Map []))) $ mktype mn <$> fields,
                      ms <- zip (("value" ++) . show <$> ([0..] :: [Int])) fields'
                      = ((\i -> CppVariableIntroduction i [] Nothing) <$> ms) ++
                        [ CppFunction name [] ms Nothing [CppConstructor] (CppBlock [])
                        , CppFunction name [] [] Nothing [CppConstructor, CppDelete] CppNoOp
                        ]
                    | otherwise = []
        go _ = ([], Nothing)

      qual :: Qualified ProperName -> String
      qual name = qualifiedToStr' (Ident . runProperName) name

      addNamespace :: String -> String -> String
      addNamespace ns s | ':' `elem` s,
                          (s1,s2) <- splitAt (last $ findIndices (==':') s) s = s1 ++ ':' : ns ++ ':' : s2
                        | otherwise = ns ++ "::" ++ s

  unaryToCpp :: UnaryOp -> CppUnaryOp
  unaryToCpp Negate = CppNegate
  unaryToCpp Not = CppNot
  unaryToCpp BitwiseNot = CppBitwiseNot

  literalToValueCpp :: Literal (CI.Expr Ann) -> m Cpp
  literalToValueCpp (NumericLiteral n) = return $ CppNumericLiteral n
  literalToValueCpp (CharLiteral c) = return $ CppStringLiteral [c] -- TODO: Create CppCharLiteral
  literalToValueCpp (StringLiteral s) = return $ CppStringLiteral s
  literalToValueCpp (BooleanLiteral b) = return $ CppBooleanLiteral b
  literalToValueCpp (ArrayLiteral xs) = CppArrayLiteral <$> mapM exprToCpp xs
  literalToValueCpp (ObjectLiteral ps) = CppObjectLiteral <$> mapM (sndM exprToCpp) ps

  -- |
  -- Shallow copy an object.
  --
  extendObj :: Cpp -> [(String, Cpp)] -> m Cpp
  extendObj = error "Extend obj TBD"

  -- |
  --
  unApp :: CI.Expr Ann -> [CI.Expr Ann] -> (CI.Expr Ann, [CI.Expr Ann])
  unApp (CI.App _ val args1) args2 = unApp val (args1 ++ args2)
  unApp other args = (other, args)

  -- |
  -- Avoiding templated args - a C++14 feature - for now by converting the decl to a
  -- passthrough function.
  --
  varDeclToFn :: String -> CI.Expr Ann -> T.Type -> [(String, Int)] -> [CppQualifier] -> m Cpp
  varDeclToFn name expr ty tparams qs = do
    e' <- exprToCpp expr
    let tparams' = flip Template [] . fst <$> tparams
        block = CppBlock [CppReturn (CppApp (asTemplate tparams' e') [CppVar "arg"])]
        typ = mktype mn ty
    return $ CppFunction name
                         tparams
                         [("arg", argtype typ)]
                         (rettype typ)
                         qs
                         block
  -- |
  -- Generate code in the simplified C++11 intermediate representation for a reference to a
  -- variable that may have a qualified name.
  --
  qualifiedToCpp :: (a -> Ident) -> Qualified a -> Cpp
  qualifiedToCpp f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = CppVar . runIdent $ f a
  qualifiedToCpp f (Qualified (Just mn') a) | mn /= mn' = CppAccessor Nothing (identToCpp $ f a) (CppScope (moduleNameToCpp mn'))
  qualifiedToCpp f (Qualified _ a) = CppVar $ identToCpp (f a)

  qualifiedToStr' :: (a -> Ident) -> Qualified a -> String
  qualifiedToStr' = qualifiedToStr mn

  -- TODO: fix when proper Meta info added
  isDict :: CI.Expr Ann -> Bool
  isDict (CI.Var (_, _, Nothing, Nothing) (Qualified (Just _) _)) = True
  isDict (CI.Var (_, _, Nothing, Nothing) (Qualified Nothing (Ident name))) | "__dict_" `isPrefixOf` name = True
  isDict (CI.App (_, _, Nothing, Nothing) -- superclass dictionary
           (CI.Accessor (_, _, Nothing, Nothing)
             (CI.Literal (_, _, Nothing, Nothing) _)
               (CI.Var (_, _, Nothing, Nothing) _))
                 [CI.Var (_, _, Nothing, Nothing) _]) = True
  isDict _ = False

  tyFromExpr :: CI.Expr Ann -> Maybe T.Type
  tyFromExpr expr = go expr
    where
    go (CI.AnonFunction (_, _, t, _) _ _) = t
    go (CI.App (_, _, t, _) _ _) = t
    go (CI.Var (_, _, t, _) _) = t
    go (CI.Literal (_, _, t, _) _) = t
    go (CI.Accessor (_, _, t, _) _ _) = t
    go _ = Nothing

  tyFromExpr' :: CI.Expr Ann -> Maybe Type
  tyFromExpr' expr = tyFromExpr expr >>= mktype mn

  asTemplate :: [Type] -> Cpp -> Cpp
  asTemplate [] cpp = cpp
  asTemplate ps (CppVar name) = CppVar (name ++ '<' : intercalate "," (runType <$> ps) ++ ">")
  asTemplate ps (CppAccessor t prop cpp)
    | Just (Native t' _) <- t = CppAccessor (Just (Native t' ps)) prop cpp
    | otherwise = CppAccessor (Just (Native [] ps)) prop cpp
  asTemplate _ cpp = cpp

  -- |
  -- Find a type class instance in scope by name, retrieving its class name and construction types.
  --
  findInstance :: Qualified Ident -> Maybe (Qualified ProperName, [Maybe Type])
  findInstance ident@(Qualified (Just mn') _)
    | Just dict <- M.lookup (ident, Just mn) (typeClassDictionaries env),
      classname <- TCD.tcdClassName dict,
      tys <- mktype mn' <$> TCD.tcdInstanceTypes dict
      = Just (classname, tys)
  findInstance _ = Nothing

  -- |
  -- Find a class in scope by name, retrieving its list of constraints, function names and types.
  --
  findClass :: Qualified ProperName -> Maybe ([String], [T.Constraint], [(String, T.Type)])
  findClass name
    | Just (params, fns, constraints) <- M.lookup name (typeClasses env),
      fns' <- (\(i,t) -> (identToCpp i, t)) <$> fns
      = Just (fst <$> params, constraints, (sortBy (compare `on` fst) fns'))
  findClass _ = Nothing

  toHeader :: [Cpp] -> [Cpp]
  toHeader = catMaybes . map go
    where
    go :: Cpp -> Maybe Cpp
    go cpp@(CppNamespace{}) = Just cpp
    go cpp@(CppStruct{}) = Just cpp
    go (CppFunction name [] args rtyp qs _) = Just (CppFunction name [] args rtyp qs CppNoOp)
    go cpp@(CppFunction{}) = Just cpp
    go cpp@(CppVariableIntroduction{}) = Just cpp
    go _ = Nothing

  toBody :: [Cpp] -> [Cpp]
  toBody = catMaybes . map go
    where
    go :: Cpp -> Maybe Cpp
    go cpp@(CppFunction _ [] _ _ _ _) = Just cpp
    go _ = Nothing

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False

nativeMain :: Cpp
nativeMain = CppFunction "main"
               []
               [ ([], Just (Native "int" []))
               , ([], Just (Native "char *[]" []))
               ]
               (Just (Native "int" []))
               []
               (CppBlock [ CppApp (CppAccessor Nothing "main" (CppScope "Main")) []
                         , CppReturn (CppNumericLiteral (Left 0))
                         ])
