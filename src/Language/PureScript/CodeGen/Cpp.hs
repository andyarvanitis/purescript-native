-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
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
    moduleToCpp,
    P.prettyPrintCpp
) where

import Data.List
import Data.Char
import Data.Function (on)
import Data.Maybe
import Data.Tuple (swap)
import qualified Data.Traversable as T (traverse)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class
import Control.Monad (when, liftM2)

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
import qualified Language.PureScript.CoreImp.Traversals as CI
import qualified Language.PureScript.Types as T
import qualified Language.PureScript.TypeClassDictionaries as TCD
import qualified Language.PureScript.Pretty.Cpp as P
import qualified Language.PureScript.Pretty.Common as P
import qualified Data.Graph as G

import Debug.Trace

data DeclLevel = TopLevel | InnerLevel deriving (Eq, Show);

-- |
-- Generate code in the simplified C++11 intermediate representation for all declarations in a
-- module.
--
moduleToCpp :: forall m mode. (Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => Environment -> Module (CI.Decl Ann) -> m [Cpp]
moduleToCpp env (Module _ mn imps exps foreigns decls) = do
  additional <- asks optionsAdditional
  cppImports <- T.traverse (pure . runModuleName) . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ imps
  let cppImports' = "PureScript" : cppImports
  cppDecls <- mapM (declToCpp TopLevel) decls
  optimized <- T.traverse optimize (concatMap expandSeq cppDecls)
  let optimized' = removeCodeAfterReturnStatements <$> optimized
  let isModuleEmpty = null exps
  datas <- modDatasToCpps
  synonyms <- modSynonymsToCpp
  let moduleHeader = fileBegin "HH"
                  ++ P.linebreak
                  ++ ((\i -> CppInclude i i) <$> cppImports')
                  ++ (if not (null foreigns)
                        then [CppInclude [] (runModuleName mn ++ "_ffi")]
                        else []
                     )
                  ++ P.linebreak
                  ++ headerDefsBegin
                  ++ [CppNamespace (runModuleName mn) $
                       (CppUseNamespace <$> cppImports') ++ P.linebreak
                                                         ++ synonyms -- TODO: dependency sort these
                                                         ++ datas    -- two groups (and maybe others)
                                                         ++ toHeader optimized'
                                                         ++ toHeaderFns optimized'
                     ]
                  ++ P.linebreak
                  ++ headerDefsEnd
                  ++ P.linebreak
                  ++ fileEnd "HH"
  let bodyCpps = toBody optimized'
      moduleBody = fileBegin "CC"
                ++ P.linebreak
                ++ CppInclude (runModuleName mn) (runModuleName mn) : P.linebreak
                ++ (if null bodyCpps
                      then []
                      else [CppNamespace (runModuleName mn) $
                             (CppUseNamespace <$> cppImports') ++ P.linebreak ++ bodyCpps])
                ++ P.linebreak
                ++ (if isMain mn then [nativeMain] else [])
                ++ fileEnd "CC"
  return $ case additional of
    MakeOptions _ -> moduleHeader ++ CppEndOfHeader : moduleBody
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
  -- Typeclass instance definitions
  --
  declToCpp TopLevel (CI.VarDecl _ ident expr)
    | Just _ <- findInstance (Qualified (Just mn) ident) = instanceDeclToCpp ident expr
  declToCpp TopLevel (CI.Function (_, _, Just T.ConstrainedType{}, _) ident _ [CI.Return _ expr])
    | Just _ <- findInstance (Qualified (Just mn) ident) = instanceDeclToCpp ident expr

  declToCpp TopLevel (CI.VarDecl _ ident expr)
    | Just ty <- tyFromExpr expr,
      (Just EffectFunction{}) <- mktype mn ty = fnDeclCpp ty ident [] expr []

  -- Note: for vars, avoiding templated args - a C++14 feature - for now
  declToCpp TopLevel (CI.VarDecl _ ident expr)
    | Just ty <- tyFromExpr expr,
      tmplts@(_:_) <- templparams' (mktype mn ty)
      -- ty'@(Just Function{}) <- mktype mn ty,
      = fnDeclCpp ty ident tmplts expr [CppInline]

  declToCpp _ (CI.VarDecl (_, _, ty, _) ident expr) = do
    expr' <- exprToCpp expr
    -- Remove lambda capture
    let cpp | (CppApp (CppLambda _ args rtyp block) larg) <- expr' = CppApp (CppLambda [] args rtyp block) larg
            | (CppLambda _ args rtyp block) <- expr' = CppLambda [] args rtyp block
            | otherwise = expr'
    return $ CppVariableIntroduction (identToCpp ident, ty >>= mktype mn) [] [] (Just cpp)

  -- TODO: fix when proper Meta info added
  declToCpp TopLevel (CI.Function _ _ [Ident "dict"]
    [CI.Return _ (CI.Accessor _ _ (CI.Var _ (Qualified Nothing (Ident "dict"))))]) =
    return CppNoOp

  -- Strip contraint dict parameters from function decl
  declToCpp lvl (CI.Function (ss, com, Just ty@(T.ConstrainedType ts _), _) ident allArgs@(arg:_) [body])
    | "__dict_" `isPrefixOf` identToCpp arg = do
    let fn' = drop' (min (length allArgs) (length ts)) ([], body)
    declToCpp lvl $ CI.Function (ss, com, Just ty, Nothing) ident (fst fn') [snd fn']
      where
        drop' :: Int -> ([Ident], CI.Statement Ann) -> ([Ident], CI.Statement Ann)
        drop' n (args, CI.Return _ (CI.AnonFunction _ args' [body'])) | n > 0 = drop' (n-1) (args ++ args', body')
        drop' _ a = a

  declToCpp TopLevel (CI.Function (_, comms, Just ty, _) ident [arg] body) | Just typ' <- mktype mn ty = do
    let rankNs = concatMap getRankNs body
    let body' = if null rankNs then body else replaceRankNs rankNs <$> body
    block <- CppBlock <$> mapM statmentToCpp body'
    let typ = Just typ'
        argType = argtype typ
        args = [(identToCpp arg, argType)]
        tmplts = tmpltsReplFromRight (templparams' typ) (filter parameterized $ templparams' typ)
        retType = rettype typ
        block' = handleRankNCpps (templateVars typ') block
        f = CppFunction (identToCpp ident)
                        tmplts
                        args
                        retType
                        []
                        block'
        f' | Just aty' <- argType, everythingOnTypes (||) (== AutoType) aty' =
             CppVariableIntroduction (identToCpp ident, Nothing)
                                     tmplts
                                     []
                                     (Just (CppLambda [] args retType block'))
           | otherwise = f
    return (CppComment comms f')
    where
    parameterized :: (String, Int) -> Bool
    parameterized (_, n) | n > 0 = True
    parameterized _ = False

  -- Point-free top-level functions
  declToCpp TopLevel (CI.Function (_, comms, Just ty, _) ident [] [(CI.Return _ e)])
    | tmplts@(_:_) <- templparams' (mktype mn ty) = do
      cpp' <- fnDeclCpp ty ident tmplts e []
      return (CppComment comms cpp')

  -- TODO:
  declToCpp TopLevel (CI.Function (_, _, Just _, _) ident [] [CI.Return _ _]) = error $ show ident

  -- C++ doesn't support nested functions, so use lambdas
  declToCpp InnerLevel (CI.Function (_, _, Just ty, _) ident [arg] body) = do
    block <- CppBlock <$> mapM statmentToCpp body
    return $ CppVariableIntroduction (identToCpp ident, Nothing) [] [] (Just (asLambda block))
    where
    asLambda block' =
      let typ = mktype mn ty
          tmplts = maybe [] (map Just . templateVars) typ
          argName = identToCpp arg
          argType = argtype typ
          retType = rettype typ
          replacements = replacementPair (argName, argType) <$> tmplts
          replacedBlock = replaceTypes replacements block'
          removedTypes = zip tmplts (repeat Nothing)
          replacedArgs = applyChanges removedTypes argType
          replacedRet = applyChanges removedTypes retType
      in CppLambda [CppCaptureAll] [(argName, replacedArgs)] replacedRet replacedBlock
      where
      replacementPair :: (String, Maybe Type) -> Maybe Type -> (Maybe Type, Maybe Type)
      replacementPair (argName, argType) typ'
        | argType == typ' = (argType, Just (DeclType argName))
      replacementPair (_, argType) _ = (argType, Nothing)

      replaceTypes :: [(Maybe Type, Maybe Type)] -> Cpp -> Cpp
      replaceTypes [] = id
      replaceTypes typs = everywhereOnCpp replace
        where
        replace (CppLambda cps [(argn, argt)] rett b) = CppLambda cps [(argn, applyChanges typs argt)] (applyChanges typs rett) b
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
    return $ CppVariableIntroduction (identToCpp ident, Nothing) [] []
               (Just (CppLambda [CppCaptureAll] [(identToCpp arg, Nothing)] Nothing block))

  declToCpp InnerLevel CI.Function{} = return CppNoOp -- TODO: Are these possible?
  declToCpp _ (CI.Function (_, _, Just _, _) _ (_:_) _) = return CppNoOp -- TODO: support non-curried functions?

  -- |
  -- Typeclass declaration
  --
  declToCpp TopLevel (CI.Constructor (_, comms, _, Just IsTypeClassConstructor) _ (Ident ctor) _)
    | Just (params, constraints, fns) <- findClass (Qualified (Just mn) (ProperName ctor)) = do
    let tmplts = runType . mkTemplate <$> params
        fnTemplPs = nub $ (concatMap (templparams' . mktype mn . snd) fns) ++
                          (concatMap constraintParams constraints)
        classTemplParams = zip tmplts $ fromMaybe 0 . flip lookup fnTemplPs <$> tmplts
    cpps' <- mapM (toCpp tmplts) fns
    let struct' = CppStruct (ctor, classTemplParams)
                            []
                            (constraintStrings [] <$> constraints)
                            cpps'
                            []
    return (CppComment comms struct')
    where
    tmpParams :: [(String, T.Type)] -> [(String, Int)]
    tmpParams fns = concatMap (templparams' . mktype mn . snd) fns
    constraintParams :: T.Constraint -> [(String, Int)]
    constraintParams (cname@(Qualified _ _), cps)
      | Just (ps, constraints', fs) <- findClass cname,
        ps' <- runType . mkTemplate <$> ps,
        tps@(_:_) <- filter ((`elem` ps') . fst) (tmpParams fs),
        ts@(_:_) <- (\p -> case find ((== p) . fst) tps of
                             Just (_, n) -> n
                             _ -> 0) <$> ps' = let cps' = typestr mn <$> cps in
                                               zip cps' ts ++ concatMap constraintParams constraints'
    constraintParams (cname, _)
      | Just (_, constraints'@(_:_), _) <- findClass cname = concatMap constraintParams constraints'
    constraintParams _ = []

    toCpp :: [String] -> (String, T.Type) -> m Cpp
    toCpp tmplts (name, ty)
      | ty'@(Just _) <- mktype mn ty,
        Just atyp <- argtype ty',
        Just rtyp <- rettype ty'
      = return $ CppFunction name
                             (filter ((`notElem` tmplts) . fst) $ templparams' ty')
                             [([], Just atyp)]
                             (Just rtyp)
                             [CppStatic]
                             CppNoOp
    toCpp tmplts (name, ty)
      | typ <- mktype mn ty,
        tmplts'@(_:_) <- filter ((`notElem` tmplts) . fst) $ templparams' typ = do
        return $ CppVariableIntroduction (name, typ) tmplts' [CppStatic] Nothing
    toCpp _ (name, ty) = return $ CppVariableIntroduction (name, mktype mn ty) [] [CppStatic] Nothing

  -- |
  -- data declarations (to omit)
  --
  declToCpp _ (CI.Constructor (_, _, _, Just IsNewtype) _ _ _) = return CppNoOp
  declToCpp _ (CI.Constructor _ _ _ []) = return CppNoOp
  declToCpp _ (CI.Constructor (_, _, _, _) _ _ _) = return CppNoOp

  declToCpp TopLevel _ = return CppNoOp -- TODO: includes Function IsNewtype

  instanceDeclToCpp :: Ident -> CI.Expr Ann -> m Cpp
  instanceDeclToCpp ident expr
    | Just (classname@(Qualified (Just classmn) (ProperName unqualClass)), typs) <- findInstance (Qualified (Just mn) ident),
      Just (params, constraints, fns) <- findClass classname = do
    let (_, fs) = case expr of
                    CI.App{} -> unApp expr []
                    CI.AnonFunction _ _ [CI.Return _ e'@CI.App{}] -> unApp e' []
                    _ -> error $ "Unknown expression type:\n" ++ show expr
        fs' = filter (isNormalFn) fs
        typs' = catMaybes typs
        tmplts = nub . sort $ concatMap templparams typs'
    cpps <- mapM (toCpp tmplts) (zip fns fs')
    when (length fns /= length fs') (error $ "Instance function list mismatch! " ++ '(': show ident ++ ")\n"
                                          ++ show fns ++ "\n -vs- \n" ++ show fs')
    let isDataTypeCtor' = isDataTypeCtor typs'
        tmplts' = if isDataTypeCtor'
                    then tmpltsReplFromRight
                           (nub . sort $ concatMap templparams typs')
                           (templatesFromKinds typs')
                    else tmplts
        typs'' = if isDataTypeCtor'
                   then (TypeConstructor (P.dotsTo '_' $ runModuleName mn)) <$> typs'
                   else typs'
        tymap = zip (mkTemplate <$> params) typs''
        struct = CppStruct (unqualClass, tmplts')
                           typs''
                           (constraintStrings tymap <$> constraints)
                           cpps
                           []
    return $ if classmn == mn
               then struct
               else CppNamespace ("::" ++ runModuleName classmn) [CppUseNamespace (runModuleName mn), struct]
    where
    toCpp :: [(String, Int)] -> ((String, T.Type), CI.Expr Ann) -> m Cpp
    toCpp tmplts ((name, ty'), CI.AnonFunction ann'@(_, _, Just ty, _) ags sts) = do
      fn' <- declToCpp TopLevel $ CI.Function ann' (Ident name) ags sts
      let cpp = addQual CppStatic (removeTemplates tmplts fn')
      return (case mktype mn ty' of
                Just Function{} -> cpp
                Just _ -> asLambda cpp
                _ -> cpp)
      where
      asLambda :: Cpp -> Cpp
      asLambda (CppFunction _ tmplts' args rty _ body) =
        CppVariableIntroduction (name, mktype mn ty) tmplts' [CppStatic] (Just $ CppLambda [] args rty body)
      asLambda (CppComment comms cpp') = CppComment comms (asLambda cpp')
      asLambda cpp = cpp

    toCpp tmplts ((name, _), e) -- Note: for vars, avoiding templated args - a C++14 feature - for now
      | Just ty <- tyFromExpr e,
        typ@(Just Function{}) <- mktype mn ty,
        tmplts'@(_:_) <- templparams' typ
       = fnDeclCpp ty (Ident name) (filter (`notElem` tmplts) tmplts') e [CppInline, CppStatic]
    toCpp _ ((name, _), e)
      | Just ty <- tyFromExpr e,
        Just Function{} <- mktype mn ty =
        fnDeclCpp ty (Ident name) [] e [CppInline, CppStatic]
    toCpp _ ((name, _), e@(CI.Literal _ NumericLiteral{}))
      | Just ty <- tyFromExpr e = literalCpp name ty e
    toCpp _ ((name, _), e@(CI.Literal _ BooleanLiteral{}))
      | Just ty <- tyFromExpr e = literalCpp name ty e
    toCpp _ ((name, _), e@(CI.Literal _ CharLiteral{}))
      | Just ty <- tyFromExpr e = literalCpp name ty e
    toCpp tmplts ((name, _), e)
      | Just ty <- tyFromExpr e,
        typ <- mktype mn ty,
        tmplts'@(_:_) <- templparams' typ = do
        e' <- exprToCpp e
        return $ CppVariableIntroduction (name, mktype mn ty) (filter (`notElem` tmplts) tmplts') [CppStatic] (Just e')
    toCpp _ ((name, _), e)
      | Just ty <- tyFromExpr e = do
        e' <- exprToCpp e
        return $ CppVariableIntroduction (name, mktype mn ty) [] [CppStatic] (Just e')
    toCpp _ ((name, _), e) = return $ error $ (name ++ " :: " ++ show e ++ "\n")

    literalCpp :: String -> T.Type -> CI.Expr Ann -> m Cpp
    literalCpp name ty e = do
        e' <- exprToCpp e
        return $ CppVariableIntroduction (name, mktype mn ty) [] [CppStatic] (Just e')

    isDataTypeCtor :: [Type] -> Bool
    isDataTypeCtor ts = any go ts
      where
      go :: Type -> Bool
      go (Native nname nts@(_:_))
        | Just (_, tk) <- M.lookup (Qualified (Just mn) (ProperName nname)) (types env),
          DataType ps _ <- tk = length ps /= length nts
      go _ = False
    isDataTypeCtor _ = False

    templatesFromKinds :: [Type] -> [(String, Int)]
    templatesFromKinds ts = concatMap go ts
      where
      go :: Type -> [(String, Int)]
      go (Native nname nts@(_:_))
        | Just (_, tk) <- M.lookup (Qualified (Just mn) (ProperName nname)) (types env),
          DataType ps _ <- tk = catMaybes $ swapNames <$> zip (templateFromKind <$> ps) nts
      go _ = []
      swapNames :: ((String, Int), Type) -> Maybe (String, Int)
      swapNames ((_, n), (Template t _)) = Just (runType (Template t []), n)
      swapNames _ = Nothing
    templatesFromKinds _ = []

    addQual :: CppQualifier -> Cpp -> Cpp
    addQual q (CppFunction name tmplts args rty qs cpp) = CppFunction name tmplts args rty (q : qs) cpp
    addQual q (CppVariableIntroduction name tmplts qs cpp) = CppVariableIntroduction name tmplts (q : qs) cpp
    addQual q (CppComment comms cpp') = CppComment comms (addQual q cpp')
    addQual _ cpp = cpp

    isNormalFn :: (CI.Expr Ann) -> Bool
    isNormalFn (CI.AnonFunction _ [Ident "__unused"] [CI.Return _ e]) | Nothing <- tyFromExpr e = False
    isNormalFn _ = True

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
    addTypes (CI.IsTagOf (_, _, ty, _) ctor e) sts = do
      e' <- exprToCpp e
      return (typeAccessors e' sts)
      where
      typeAccessors :: Cpp -> Cpp -> Cpp
      typeAccessors acc = everywhereOnCpp convert
        where
        convert :: Cpp -> Cpp
        convert (CppAccessor Nothing prop cpp) | cpp == acc = CppAccessor qtyp prop cpp
        convert cpp = cpp
        tmplts :: [Type]
        tmplts = maybe [] templateVars (ty >>= mktype mn)
        qtyp :: Maybe Type
        qtyp = Just (Native (qualifiedToStr' (Ident . runProperName) ctor) tmplts)
    addTypes _ cpp = return cpp
  statmentToCpp (CI.Return _ expr) =
    CppReturn <$> exprToCpp expr
  statmentToCpp (CI.Throw _ msg) =
    return . CppThrow $ CppApp (CppVar "runtime_error") [CppStringLiteral msg]
  statmentToCpp (CI.Label _ lbl stmnt) =
    CppLabel lbl <$> statmentToCpp stmnt
  statmentToCpp (CI.Comment _ _) = return CppNoOp

  loopStatementToCpp :: CI.LoopStatement Ann -> m Cpp
  loopStatementToCpp (CI.Break _ lbl) = return . CppBreak $ fromMaybe "" lbl
  loopStatementToCpp (CI.Continue _ lbl) = return . CppContinue $ fromMaybe "" lbl
  loopStatementToCpp (CI.Statement s) = statmentToCpp s

  exprToCpp :: CI.Expr Ann -> m Cpp

  exprToCpp (CI.Literal (_, _, Just ty, _) lit@ArrayLiteral{})
    | Just typ <- mktype mn ty = do
      lit' <- literalToValueCpp lit
      return (CppVar (runType typ ++ P.prettyPrintCpp [lit']))
  exprToCpp (CI.Literal _ lit) =
    literalToValueCpp lit

  -- TODO: Change how this is done when proper Meta info added
  exprToCpp (CI.Accessor _ (CI.Literal _ (StringLiteral name)) expr)
    | "__superclass_" `isPrefixOf` name = exprToCpp expr
  exprToCpp (CI.Accessor _ prop@(CI.Literal _ (StringLiteral name)) expr@(CI.Var (_, _, ty, _) _)) = do
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
  exprToCpp (CI.AnonFunction _ (_:_:_) _) = return CppNoOp -- TODO: non-curried lambdas
  exprToCpp (CI.AnonFunction (_, _, ty, _) args stmnts') = do
    let typ = ty >>= mktype mn
        args' | [arg] <- args = [(identToCpp arg, argtype typ)]
              | otherwise = []
    body <- CppBlock <$> mapM statmentToCpp stmnts'
    return $ CppLambda [CppCaptureAll] args' (rettype typ) body

  -- |
  -- Function application
  --
  exprToCpp (CI.App _ e [CI.Var _ (Qualified (Just (ModuleName [ProperName "Prim"])) (Ident "undefined"))]) =
    exprToCpp e
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
            tmplts = maybe [] getDataTypeArgs (mktype mn ty >>= getDataType qname)
            val = CppDataConstructor qname tmplts in
        if argsNotApp > 0
          then let argTypes = maybe [] (init . fnTypesN fieldCount) (mktype mn ty) in
               return (CppPartialApp val args' argTypes argsNotApp)
          else return (CppApp val args')

      CI.Var (_, _, _, Just IsTypeClassConstructor) _ -> return CppNoOp
      CI.Var (_, _, Just _, _) (Qualified (Just _) _) -> fnAppCpp e
      CI.Var (_, _, Nothing, _) ident -> (case findInstance ident of
                                            Nothing -> fnAppCpp e
                                            _ -> exprToCpp f)
      -- TODO: verify this
      _ -> flip (foldl (\fn a -> CppApp fn [a])) args' <$> exprToCpp f

  exprToCpp (CI.Var (_, _, ty, Just (IsConstructor _ fields)) ident) =
    let qname = qualifiedToStr' id ident
        tmplts = maybe [] getDataTypeArgs (ty >>= mktype mn >>= getDataType qname)
        fieldCount = length fields in
    return $ if fieldCount == 0
               then CppApp (CppDataConstructor (qualifiedToStr' id ident) tmplts) []
               else let argTypes = maybe [] (init . fnTypesN fieldCount) (ty >>= mktype mn) in
                    CppPartialApp (CppDataConstructor (qualifiedToStr' id ident) tmplts) [] argTypes fieldCount

  exprToCpp (CI.Var (_, _, ty, Just IsNewtype) ident) =
    let qname = qualifiedToStr' id ident
        tmplts = maybe [] getDataTypeArgs (ty >>= mktype mn >>= getDataType qname)
        argTypes = maybe [] (init . fnTypesN 1) (ty >>= mktype mn)
    in return (CppPartialApp (CppDataConstructor (qualifiedToStr' id ident) tmplts) [] argTypes 1)

  -- Typeclass instance dictionary
  exprToCpp (CI.Var (_, _, Nothing, Nothing) ident@(Qualified (Just _) (Ident instname)))
    | Just (qname@(Qualified (Just mn') (ProperName cname)), types') <- findInstance ident,
      Just (params, _, fns) <- findClass qname
    = let fs' = (\(f,t) -> (f, mktype mn t)) <$> fns
      in return $ CppInstance (runModuleName mn') ([cname], fs') instname (zip params types')

 -- Constraint typeclass dictionary
 -- TODO: Make sure this pattern will not change in PS
  exprToCpp (CI.Var (_, _, Nothing, Nothing) (Qualified Nothing (Ident name)))
    | Just prefixStripped <- stripPrefix "__dict_"  name,
      '_' : reversedName <- dropWhile isNumber (reverse prefixStripped),
      cname <- reverse $ takeWhile (not . isPunctuation) reversedName,
      mname <- reverse . drop 1 $ dropWhile (not . isPunctuation) reversedName,
      mnames <- words (P.dotsTo ' ' mname),
      Just (params, supers, fns) <- findClass (Qualified (Just (ModuleName (ProperName <$> mnames))) (ProperName cname))
    = let superFns = getFns supers
          fs' = (\(f,t) -> (f, mktype mn t)) <$> fns ++ superFns
      in return $ CppInstance mname (cname : (show . fst <$> supers), fs') [] (zip params (Just . mkTemplate <$> params))
    where
    getFns :: [T.Constraint] -> [(String, T.Type)]
    getFns = concatMap go
      where
      go :: T.Constraint -> [(String, T.Type)]
      go cls | Just (_, clss, fns) <- findClass (fst cls) = fns ++ getFns clss
      go _ = []

  exprToCpp (CI.Var _ ident) = do
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
        tmplts = maybe [] getDataTypeArgs (ty >>= mktype mn >>= getDataType qname)
        val = CppData qname tmplts in
    flip CppInstanceOf val <$> exprToCpp expr

  modDatasToCpps :: m [Cpp]
  modDatasToCpps
    | ds@(_:_) <- M.toList
                 . M.mapWithKey (\_ a -> snd a)
                 . M.filterWithKey (\(Qualified mn' _) _ -> mn' == Just mn)
                 . M.filter (isData . snd)
                 $ types env = do
      let types' = dataTypes ds
          (ctors, aliases) = dataValueCtors ds
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
        -- TODO: this feels too fragile
        fromStruct :: String -> Cpp -> Cpp
        fromStruct _ (CppStruct (_, (_:_)) [] [] [] []) = CppNoOp
        fromStruct _ (CppStruct _ _ _ _ [CppTypeAlias{}]) = CppNoOp
        fromStruct ns (CppStruct (name, tmplts) _ _ _ _) =
          let tmplts' = remTemplateDefaults tmplts in
          CppTypeAlias (name, tmplts') (ns ++ "::" ++ name, tmplts') "managed"
        fromStruct _ _ = CppNoOp

      dataTypes :: [(Qualified ProperName, TypeKind)] -> [Cpp]
      dataTypes ds | cpps@(_:_) <- concatMap go ds = [CppNamespace "type" cpps]
                   | otherwise = []
        where
        go :: (Qualified ProperName, TypeKind) -> [Cpp]
        -- go (_, DataType _ [_]) = []
        go (typ, DataType ts cs) =
          let tmplts = tmpltsReplFromRight
                         (flip (,) 0 . runType . mkTemplate . fst <$> ts)
                         (concatMap templateParams (snd <$> cs))
          in dataTypeCtors (qual typ) tmplts
        go _ = []

      dataTypeCtors :: String -> [(String, Int)] -> [Cpp]
      dataTypeCtors name [] =
        [CppStruct (name, []) [] [] []
          [CppFunction name [] [] Nothing [CppVirtual, CppDestructor, CppDefault] CppNoOp]]
      dataTypeCtors name [param] = [
          CppStruct (name, [param])
                    [] [] []
                    [CppFunction name [] [] Nothing [CppVirtual, CppDestructor, CppDefault] CppNoOp]
        ]
      dataTypeCtors name params@(p:ps) = [
          -- The template declaration
          CppStruct (name, addTemplateDefaults params) [] [] [] []
          -- The fully applied type constructor
        , CppStruct (name, params)
                    [] [] []
                    [CppFunction name [] [] Nothing [CppVirtual, CppDestructor, CppDefault] CppNoOp]
        ] ++ dataTypePartialCtors name [p] ps

      dataTypePartialCtors :: String -> [(String, Int)] -> [(String, Int)] -> [Cpp]
      dataTypePartialCtors name applied notApplied@(p:ps) =
          (CppStruct (name, applied)
                     (typeFromTemplate <$> applied)
                     [] []
                     [CppTypeAlias ("_", notApplied) (name, applied ++ notApplied) []])
        : dataTypePartialCtors name (applied ++ [p]) ps
        where
        typeFromTemplate :: (String, Int) -> Type
        typeFromTemplate (name, _) = Template name []
      dataTypePartialCtors _ _ _ = []

      dataValueCtors :: [(Qualified ProperName, TypeKind)] -> ([Cpp], [Cpp])
      dataValueCtors ds | cpps@(_:_) <- concatMap (fst . go) ds,
                     aliases <- catMaybes $ map (snd . go) ds = ([CppNamespace "value" cpps], aliases)
                   | otherwise = ([],[])
        where
        go :: (Qualified ProperName, TypeKind) -> ([Cpp], Maybe Cpp)
        go (typ, DataType ts cs) = (map ctorStruct cs, alias)
          where
          alias :: Maybe Cpp
          alias = Nothing
          -- alias | [_] <- cs =
          --   Just $ CppTypeAlias (qual typ, tmplts)
          --                       (P.prettyPrintCpp [flip CppData [] . runProperName . fst $ head cs], tmplts)
          --                       []
          --       | otherwise = Nothing
          tmplts :: [(String, Int)]
          tmplts = tmpltsReplFromRight
                     (flip (,) 0 . runType . mkTemplate . fst <$> ts)
                     (concatMap templateParams (snd <$> cs))
          ctorStruct :: (ProperName, [T.Type]) -> Cpp
          ctorStruct (ctor, fields) =
            CppStruct (name, tmplts) [] supers [] members
            where
            name :: String
            name = P.prettyPrintCpp [flip CppData [] $ runProperName ctor]
            supers :: [(String, [String])]
            supers  -- | [_] <- cs = []
                   | otherwise = [(addNamespace "type" (qual typ), fst <$> tmplts)]
            members :: [Cpp]
            members | fields'@(_:_) <- filter (/=(Just (Map []))) $ mktype mn <$> fields,
                      ms <- zip (("value" ++) . show <$> ([0..] :: [Int])) fields'
                      = ((\i -> CppVariableIntroduction i [] [] Nothing) <$> ms) ++
                        [ CppFunction name [] ms Nothing [CppConstructor] (CppBlock [])
                        , CppFunction name [] [] Nothing [CppConstructor, CppDelete] CppNoOp
                        ]
                    | otherwise = []
        go _ = ([], Nothing)

      templateParams :: [T.Type] -> [(String, Int)]
      templateParams = nub . concatMap (templparams' . mktype mn)

      tmpltsReplFromRight :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
      tmpltsReplFromRight t1s t2s = map go' t1s
        where
        go' :: (String, Int) -> (String, Int)
        go' t1 | Just n <- lookup (fst t1) t2s = (fst t1, n)
               | otherwise = t1

      qual :: Qualified ProperName -> String
      qual name = qualifiedToStr' (Ident . runProperName) name

      addNamespace :: String -> String -> String
      addNamespace ns s | ':' `elem` s,
                          (s1,s2) <- splitAt (last $ findIndices (==':') s) s = s1 ++ ':' : ns ++ ':' : s2
                        | otherwise = ns ++ "::" ++ s

  modSynonymsToCpp :: m [Cpp]
  modSynonymsToCpp
    | tcs <- typeClasses env,
      ds@(_:_) <-  M.toList
                 . M.filterWithKey (\k@(Qualified mn' _) _ -> mn' == Just mn && M.lookup k tcs == Nothing)
                 $ typeSynonyms env = do
      let names = qualifiedToStr' (Ident . runProperName) . fst <$> ds
          tmplts = map templateFromKind . fst . snd <$> ds
          typs = catMaybes $ mktype mn . snd . snd <$> ds
          synonyms = zip3 names tmplts typs
          synonyms' = depSortSynonyms synonyms
          cpps = (\(n,tmps,t) -> CppTypeAlias (n, tmps) (runType t, []) []) <$> synonyms'
      return cpps
    | otherwise = return []
    where
    -- |
    -- Dependency (topological) sorting of synonyms
    --
    depSortSynonyms :: [(String, [(String,Int)], Type)] ->  [(String, [(String,Int)], Type)]
    depSortSynonyms syns = reverse $
      catMaybes $ flip lookup vertexSyns <$> G.topSort (G.buildG (1, length syns) (concatMap findEdges syns))
      where
      findEdges ::  (String, [(String,Int)], Type) -> [G.Edge]
      findEdges syn@(_,_,typ) = everythingOnTypes (++) go typ
        where
        go ::  Type -> [G.Edge]
        go (Native name _)
          | Just thisVertex <- lookup syn vertexSyns',
            Just depVertex <- lookup name vertexes = [(thisVertex, depVertex)]
        go _ = []
      findEdges _ = []

      vertexes :: [(String, G.Vertex)]
      vertexes = zip ((\(name,_,_) -> name) <$> syns) [1 ..]

      vertexSyns :: [(G.Vertex, (String, [(String,Int)], Type))]
      vertexSyns = zip [1 ..] syns

      vertexSyns' :: [((String, [(String,Int)], Type), G.Vertex)]
      vertexSyns' = swap <$> vertexSyns

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
  -- Generate code in the simplified C++11 intermediate representation for a reference to a
  -- variable that may have a qualified name.
  --
  qualifiedToCpp :: (a -> Ident) -> Qualified a -> Cpp
  qualifiedToCpp f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = CppVar . runIdent $ f a
  qualifiedToCpp f (Qualified (Just mn') a) | mn /= mn' = CppAccessor Nothing (identToCpp $ f a) (CppScope (moduleNameToCpp mn'))
  qualifiedToCpp f (Qualified _ a) = CppVar $ identToCpp (f a)

  qualifiedToStr' :: (a -> Ident) -> Qualified a -> String
  qualifiedToStr' = qualifiedToStr mn

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

  fnTyFromApp' :: CI.Expr Ann -> Maybe Type
  fnTyFromApp' (CI.App (_, _, Just ty, _) val (a:_))
    | Just nextTy <- fnTyFromApp' val = Just nextTy
    | Just a' <- tyFromExpr' a,
      Just b' <- mktype mn ty = Just $ Function a' b'
    | Just t' <- mktype mn ty = Just t'
  fnTyFromApp' (CI.App (_, _, Nothing, _) val _) = fnTyFromApp' val
  fnTyFromApp' _ = Nothing

  -- TODO: add type/template info to CppVar?
  asTemplate :: [Type] -> Cpp -> Cpp
  asTemplate [] cpp = cpp
  asTemplate ps (CppVar name) = CppVar (name ++ '<' : intercalate "," (runType <$> ps) ++ ">")
  asTemplate ps (CppAccessor t prop cpp)
    | Just (Native t' _) <- t = CppAccessor (Just (Native t' ps)) prop cpp
    | otherwise = CppAccessor (Just (Native [] ps)) prop cpp
  asTemplate _ cpp = cpp

  removeTemplates :: [(String, Int)] -> Cpp -> Cpp
  removeTemplates tmplts = everywhereOnCpp remove
    where
    remove (CppFunction name ts args rtyp qs body) =
      let ts' = filter (`notElem` tmplts) ts in CppFunction name ts' args rtyp qs body
    remove (CppVariableIntroduction name ts qs val) =
      let ts' = filter (`notElem` tmplts) ts in CppVariableIntroduction name ts' qs val
    remove (CppComment comms cpp') = CppComment comms (remove cpp')
    remove other = other

  hasTemplates :: Cpp -> Bool
  hasTemplates = everythingOnCpp (||) go
    where
    go :: Cpp -> Bool
    go (CppFunction _ (_:_) _ _ _ _) = True
    go (CppVariableIntroduction _ (_:_) _ _) = True
    go (CppComment _ cpp') = go cpp'
    go _ = False

  tmpltsReplFromRight :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
  tmpltsReplFromRight t1s t2s = nub $ map go' t1s
    where
    go' :: (String, Int) -> (String, Int)
    go' t1 | Just n <- lookup (fst t1) t2s = (fst t1, n)
           | otherwise = t1

  constraintStrings :: [(Type, Type)] -> (Qualified ProperName, [T.Type]) -> (String, [String])
  constraintStrings [] (name, tys) = (qualifiedToStr' (Ident . runProperName) name, typestr mn <$> tys)
  constraintStrings tymap (name, tys) = (qualifiedToStr' (Ident . runProperName) name,
                                         runType . replace <$> catMaybes (mktype mn <$> tys))
    where
    replace :: Type -> Type
    replace t | Just t' <- lookup t tymap = t'
    replace t = t

  fnAppCpp :: CI.Expr Ann -> m Cpp
  fnAppCpp e = do
      let (f, args) = unApp e []
      f' <- exprToCpp f
      args' <- mapM exprToCpp args
      let fn = P.prettyPrintCpp [f']
          instArgs = filter (instanceArg fn) args'
          normArgs = filter normalArg args'
      let declType = (listToMaybe instArgs >>= instanceFnType fn) <|> tyFromExpr' f <|> findFnDeclType f
          exprType = fnTyFromApp' e
          allTemplates = fromMaybe [] $ templateMappings <$> (liftM2 (,) declType exprType)
          templateChanges = filter (\(a,b) -> a /= b) allTemplates
          instArgs' = fixInstArg templateChanges <$> instArgs
          instanceParams = concat $ catMaybes . map snd . getParams <$> instArgs'
          tmplts = filter (`notElem` instanceParams) (snd <$> allTemplates)
      return $ flip (foldl (\fn a -> CppApp fn [a])) (instArgs' ++ normArgs) (asTemplate tmplts f')
    where
      instanceArg :: String -> Cpp -> Bool
      instanceArg fname (CppInstance _ (_, fns) _ _) | Just _ <- lookup (P.stripScope fname) fns = True
      instanceArg fname (CppApp i@CppInstance{} _) = instanceArg fname i
      instanceArg _ _ = False

      normalArg :: Cpp -> Bool
      normalArg CppInstance{} = False
      normalArg (CppApp CppInstance{} _) = False
      normalArg _ = True

      instanceFnType :: String -> Cpp -> Maybe Type
      instanceFnType fname (CppInstance _ (_, fns) _ ps)
        | Just (Just typ) <- lookup (P.stripScope fname) fns = Just $ everywhereOnTypes go typ
        where
        go t@(Template name _) | Just (Just t') <- lookup name ps,
                                   [(_,t2)] <- templateReplacements (t, t') = t2
        go t = t
      instanceFnType fname (CppApp i@CppInstance{} _) = instanceFnType fname i
      instanceFnType _ _ = Nothing

      findFnDeclType :: CI.Expr Ann -> Maybe Type
      findFnDeclType (CI.Var _ (Qualified mn' ident))
        | (Just ty) <- findValue (fromMaybe mn mn') ident, typ@(Just _) <- mktype mn ty = typ
      findFnDeclType _ = Nothing

      getParams :: Cpp -> [(String, Maybe Type)]
      getParams (CppInstance _ _ _ ps) = ps
      getParams _ = []

      fixInstArg :: [(Type, Type)] -> Cpp -> Cpp
      fixInstArg mappings (CppInstance mn' cls iname ps) = CppInstance mn' cls iname (go <$> ps)
        where
        go :: (String, Maybe Type) -> (String, Maybe Type)
        go (pn, Just pty) | Just pty' <- lookup pty mappings = (pn, Just pty')
        go p = p
      fixInstArg mappings (CppApp i@CppInstance{} _) = fixInstArg mappings i
      fixInstArg _ cpp = cpp

  fnDeclCpp :: T.Type -> Ident -> [(String, Int)] -> CI.Expr Ann -> [CppQualifier] -> m Cpp
  fnDeclCpp ty ident tmplts expr qs | Just typ' <- mktype mn ty = do
      let typ = Just typ'
          atyp = argtype typ
      appExpr <- toApp atyp expr
      let appExpr' = handleRankNCpps (templateVars typ') appExpr
      return $ CppFunction (identToCpp ident)
                           tmplts
                           (fnArg atyp)
                           (rettype typ)
                           qs
                           (CppBlock [CppReturn appExpr'])
      where
      toApp :: Maybe Type -> CI.Expr Ann -> m Cpp
      toApp atyp e
        | (CI.Var _ qid@(Qualified mname vid)) <- e,
           Just ty' <- findValue (fromMaybe mn mname) vid =
           fnAppCpp $ CI.App (Nothing, [], Just ty, Nothing)
                             (CI.Var (Nothing, [], Just ty', Nothing) qid)
                             (appArg atyp)
        | (CI.Var ann'@(_, _, Just _, _) qid) <- e =
          fnAppCpp $ CI.App (Nothing, [], Just ty, Nothing)
                            (CI.Var ann' qid)
                            (appArg atyp)
        | otherwise = CppApp <$> fnAppCpp e <*> appArg' atyp

      fnArg :: Maybe Type -> [(String, Maybe Type)]
      fnArg Nothing = []
      fnArg atyp = [(P.mkarg, atyp)]

      appArg :: Maybe Type -> [CI.Expr Ann]
      appArg Nothing = []
      appArg _ = [CI.Var nullAnn (Qualified Nothing (Ident P.mkarg))]

      appArg' :: Maybe Type -> m [Cpp]
      appArg' Nothing = return []
      appArg' _ = return [CppVar P.mkarg]
  fnDeclCpp ty ident _ _ _ =
    error ("Cannot derive type information from " ++ show ident ++ " :: " ++ show ty)

  -- |
  -- Find a type class instance in scope by name, retrieving its class name and construction types.
  --
  -- TODO: see if we can provide typeclass to make this faster
  --
  findInstance :: Qualified Ident -> Maybe (Qualified ProperName, [Maybe Type])
  findInstance ident@(Qualified (Just mn') _)
    | Just classMap <- M.lookup (Just mn) (typeClassDictionaries env),
      dictMaps@(_:_) <- M.elems classMap,
      dicts <- concat $ M.toList <$> dictMaps,
      Just dict <- lookup ident dicts,
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
      = Just (fst <$> params, constraints, (sortBy (compare `on` normalizedName . fst) fns'))
  findClass _ = Nothing

  -- |
  -- Find a value (incl functions) in scope by name, retrieving its type
  --
  findValue :: ModuleName -> Ident -> Maybe T.Type
  findValue mname ident
    | Just (ty, _, _) <- M.lookup (mname, ident) (names env) = Just ty
  findValue _ _ = Nothing

  toHeader :: [Cpp] -> [Cpp]
  toHeader = catMaybes . map go
    where
    go :: Cpp -> Maybe Cpp
    go (CppNamespace name cpps) = Just (CppNamespace name (toHeader cpps))
    go cpp@(CppUseNamespace{}) = Just cpp
    go (CppStruct (s, []) ts@(_:_) _ ms@(_:_) [])
      | all (not . hasTemplates) ms = Just (CppStruct (s, []) ts [] [] [])
    go (CppStruct (s, []) ts@(_:_) _ ms@(_:_) [])
      | ms'@(_:_) <- catMaybes (fromConst <$> ms) = Just (CppSequence ms')
      where
      fromConst :: Cpp -> Maybe Cpp
      fromConst (CppVariableIntroduction (name, typ@(Just _)) tmps qs cpp)
        | CppStatic `elem` qs =
          Just $ CppVariableIntroduction (fullname name, typ) tmps [CppTemplSpec] cpp
      fromConst (CppFunction name tmplts args rtyp qs body) =
        Just $ CppFunction (fullname name) tmplts args rtyp (CppTemplSpec : (qs \\ [CppInline, CppStatic])) body
      fromConst (CppComment comms cpp) | Just cpp' <- fromConst cpp = Just (CppComment comms cpp')
      fromConst _ = Nothing
      fullname :: String -> String
      fullname name = s ++ '<' : intercalate "," (runType <$> ts) ++ ">::" ++ name
    go (CppStruct (s, []) ts supers ms@(_:_) [])
      | ms'@(_:_) <- fromConst <$> ms = Just (CppStruct (s, []) ts supers ms' [])
      where
      fromConst :: Cpp -> Cpp
      fromConst (CppVariableIntroduction (name, typ@(Just _)) tmps qs _) =
        CppVariableIntroduction (name, typ) tmps qs Nothing
      fromConst cpp = cpp
    go cpp@(CppStruct{}) = Just cpp
    go (CppFunction name tmplts args rtyp qs _) = Just (CppFunction name tmplts args rtyp qs CppNoOp)
    go cpp@(CppVariableIntroduction{}) = Just cpp
    go (CppComment comms cpp')
      | Just cpp <- go cpp' = Just $ case cpp of CppFunction {} -> cpp
                                                 _ -> CppComment comms cpp
    go _ = Nothing

  toHeaderFns :: [Cpp] -> [Cpp]
  toHeaderFns = catMaybes . map go
    where
    go :: Cpp -> Maybe Cpp
    go (CppNamespace name cpps) = Just (CppNamespace name (toHeaderFns cpps))
    go cpp@(CppFunction _ (_:_) _ _ _ _) = Just cpp
    go (CppComment comms cpp') | Just cpp <- go cpp' = Just (CppComment comms cpp)
    go _ = Nothing

  toBody :: [Cpp] -> [Cpp]
  toBody = catMaybes . map go
    where
    go :: Cpp -> Maybe Cpp
    go (CppNamespace name cpps) =
      let cpps' = toBody cpps in
      if all isNoOp cpps'
        then Nothing
        else Just (CppNamespace name cpps')
      where
      isNoOp :: Cpp -> Bool
      isNoOp CppNoOp = True
      isNoOp (CppComment _ cpp) | isNoOp cpp = True
      isNoOp (CppUseNamespace{}) = True
      isNoOp (CppNamespace _ cpps) = all isNoOp cpps
      isNoOp (CppRaw _) = True
      isNoOp _ = False
    go cpp@(CppUseNamespace{}) = Just cpp
    go cpp@(CppFunction _ [] _ _ _ _) = Just cpp
    go (CppStruct (s, []) ts@(_:_) _ ms@(_:_) _)
      | all (not . hasTemplates) ms,
        ms'@(_:_) <- catMaybes (fromConst <$> ms) = Just (CppSequence ms')
      where
      fromConst :: Cpp -> Maybe Cpp
      fromConst (CppVariableIntroduction (name, typ@(Just _)) tmps qs cpp)
        | CppStatic `elem` qs =
          Just $ CppVariableIntroduction (fullname name, typ) tmps (CppTemplSpec:(delete CppStatic qs)) cpp
      fromConst (CppFunction name tmplts args rtyp qs body) =
        Just $ CppFunction (fullname name) tmplts args rtyp (CppTemplSpec : (qs \\ [CppInline, CppStatic])) body
      fromConst (CppComment comms cpp) | Just cpp' <- fromConst cpp = Just (CppComment comms cpp')
      fromConst _ = Nothing
      fullname :: String -> String
      fullname name = s ++ '<' : intercalate "," (runType <$> ts) ++ ">::" ++ name
    go (CppComment comms cpp') | Just commented <- go cpp' = Just (CppComment comms commented)
    go _ = Nothing

  fileBegin :: String -> [Cpp]
  fileBegin suffix = [CppRaw ("#ifndef " ++ fileModName suffix),
                      CppRaw ("#define " ++ fileModName suffix)]
  fileEnd :: String -> [Cpp]
  fileEnd suffix = [CppRaw ("#endif // " ++ fileModName suffix)]
  fileModName :: String -> String
  fileModName suffix = P.dotsTo '_' (runModuleName mn ++ '_' : suffix)

  headerDefsBegin :: [Cpp]
  headerDefsBegin = [CppRaw ("#if !defined" ++ P.parens (fileModName "CC")),
                     CppRaw "#define EXTERN(e) extern e",
                     CppRaw "#else",
                     CppRaw "#define EXTERN(e)",
                     CppRaw "#endif"]
  headerDefsEnd :: [Cpp]
  headerDefsEnd = [CppRaw "#undef EXTERN"]

  replaceRankNs :: [(Qualified Ident, T.Type)] -> CI.Statement Ann -> CI.Statement Ann
  replaceRankNs vs | (_, _, f, _) <- CI.everywhere id go id id = f
    where
    go :: CI.Expr Ann -> CI.Expr Ann
    go e@(CI.Var (_, _, Just t, _) _)
      | T.everythingOnTypes (||) (not . T.isMonoType) t = e
    go e@(CI.Var (_, _, Just t, _) v)
     | Just rankNTy <- lookup (cleanName v) vs,
       Just rankNTyp <- mktype mn rankNTy,
       Just varTyp <- mktype mn t,
       mappings@(_:_) <- templateMappings (rankNTyp, varTyp)
       = CI.App nullAnn e [CI.Var nullAnn (typevals mappings)]
    go (CI.App ann@(_, _, Just t, _) f'@(CI.Var _ v) [a'])
      | Just rankNTy <- lookup (cleanName v) vs,
        Just rankNTyp <- mktype mn rankNTy,
        Just retTyp <- mktype mn t,
        Just argTyp <- tyFromExpr' a',
        mappings@(_:_) <- templateMappings (rankNTyp, Function argTyp retTyp)
        = CI.App ann (CI.App nullAnn f' [CI.Var nullAnn (typevals mappings)]) [a']
    go e = e
    cleanName :: Qualified Ident -> Qualified Ident
    cleanName (Qualified m (Ident s))
      | rev@(c:_) <- reverse s,
        isDigit c,
        ('_': ss@(_:_)) <- dropWhile isDigit rev = Qualified m (Ident (reverse ss))
    cleanName n = n

-- TODO: what about shadowed names?
--
handleRankNCpps :: [Type] -> Cpp -> Cpp
handleRankNCpps tmplts cpp = everywhereOnCpp go cpp
  where
  go :: Cpp -> Cpp
  go cpp'@(CppPartialApp _ _ ts _)
    | rns@(_:_) <- (nub $ concatMap templateVars ts) \\ tmplts = rankNWrapper rns cpp'
  go cpp'@(CppApp (CppDataConstructor _ ts) _)
    | rns@(_:_) <- (nub $ concatMap templateVars ts) \\ tmplts = rankNWrapper rns cpp'
  go cpp' = cpp'

rankNWrapper :: [Type] -> Cpp -> Cpp
rankNWrapper tmplts cpp = CppLambda []
                                    ((\t -> ('_' : t, Just AutoType)) <$> tmplts')
                                    Nothing
                                    (CppBlock ((typeAlias <$> tmplts') ++ [CppReturn cpp]))
  where
  tmplts' :: [String]
  tmplts' = nub . sort $ runType <$> tmplts
  typeAlias :: String -> Cpp
  typeAlias t = CppTypeAlias (t, []) (runType (DeclType ('_' : t)), []) []

-- TODO: what about shadowed names?
--
getRankNs :: CI.Statement Ann -> [(Qualified Ident, T.Type)]
getRankNs | (_, _, f, _) <- CI.everything (++) (const []) go (const []) (const []) = f
  where
  go :: CI.Expr Ann -> [(Qualified Ident, T.Type)]
  go (CI.Var (_, _, Just t, _) v)
    | T.everythingOnTypes (||) (not . T.isMonoType) t = [(v, t)]
  go _ = []

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
                         , CppRaw ";"
                         , CppReturn (CppNumericLiteral (Left 0))
                         ])
