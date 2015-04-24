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
  cppDecls <- mapM declToCpp decls
  optimized <- T.traverse optimize (concatMap expandSeq cppDecls)
  let optimized' = removeCodeAfterReturnStatements <$> optimized
  let isModuleEmpty = null exps
  comments <- not <$> asks optionsNoComments
  let header = if comments && not (null coms) then CppComment coms CppNoOp else CppNoOp
  datas <- modDatasToCpps
  let moduleBody = header : (CppInclude <$> cppImports')
                   ++ [CppNamespace (runModuleName mn) $
                        (CppUseNamespace <$> cppImports') ++ P.linebreak ++ datas ++ foreigns' ++ optimized']
  return $ case additional of
    MakeOptions -> moduleBody
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
    removeFromBlock go (CppBlock sts) = CppBlock (go sts)
    removeFromBlock _  cpp = cpp

  declToCpp :: CI.Decl Ann -> m Cpp
  -- |
  -- Typeclass instance definition
  --
  declToCpp (CI.VarDecl _ ident expr)
    | Just (classname@(Qualified _ (ProperName unqualClass)), typs) <- findInstance (Qualified (Just mn) ident),
      Just (params, _, fns) <- findClass classname = do
    let (_, fs) = unApp expr []
        fs' = filter (isNormalFn) fs
        classname' = qualifiedToStr' (Ident . runProperName) classname
        params' = runType . Template <$> params
        inst = CppInstance [] (classname', fst <$> fns) [] (zip params' typs)
    cpps <- mapM toCpp (zip fns fs')
    return $ CppStruct (unqualClass, Right typs) [] cpps []
    where
    toCpp :: ((String, T.Type), CI.Expr Ann) -> m Cpp
    toCpp ((name, _), CI.AnonFunction ty ags sts) = do
      fn' <- declToCpp $ CI.Function ty (Ident name) ags sts
      return (addQual CppStatic fn')
    toCpp ((name, _), e@CI.Literal{})
      | Just ty <- tyFromExpr e = declToCpp (CI.VarDecl (Nothing, [], Just ty, Nothing) (Ident name) e)
    toCpp ((name, _), e) -- Note: for vars, avoiding templated args - a C++14 feature - for now
      | Just ty <- tyFromExpr e,
        tparams <- templparams' (mktype mn ty) = varDeclToFn name e ty tparams [CppInline, CppStatic]
    toCpp ((name, _), e)
      | Just ty <- tyFromExpr e = declToCpp (CI.VarDecl (Nothing, [], Just ty, Nothing) (Ident name) e)
    toCpp ((name, _), e) = return $ trace (name ++ " :: " ++ show e ++ "\n") CppNoOp

    addQual :: CppQualifier -> Cpp -> Cpp
    addQual q (CppFunction name tmps args rty qs cpp) = CppFunction name tmps args rty (q : qs) cpp
    addQual _ cpp = cpp

    isNormalFn :: (CI.Expr Ann) -> Bool
    isNormalFn (CI.AnonFunction _
                 [Ident "__unused"]
                 [CI.Return (_, _, Nothing, Nothing) (CI.Var (_, _, Nothing, Nothing) _)]) = False
    isNormalFn _ = True

  declToCpp (CI.VarDecl (_, _, _, _) (Ident name) expr)
    | Just ty <- tyFromExpr expr,
      ty'@(Just Function{}) <- mktype mn ty,
      tparams@(_:_) <- templparams' ty' = varDeclToFn name expr ty tparams [CppInline]

  declToCpp (CI.VarDecl (_, _, ty, _) ident expr) =
    CppVariableIntroduction (identToCpp ident, maybe [] (typestr mn) ty) . Just <$> exprToCpp expr

  -- TODO: fix when proper Meta info added
  declToCpp (CI.Function _ ident [Ident "dict"]
    [CI.Return _ (CI.Accessor _ _ (CI.Var _ (Qualified Nothing (Ident "dict"))))]) =
    return CppNoOp

  declToCpp (CI.Function (ss, com, Just (T.ConstrainedType ts ty), _) ident args [body]) = do
    let fn' = drop' (length ts) ([], body)
    declToCpp $ CI.Function (ss, com, Just ty, Nothing) ident (fst fn') [snd fn']
      where
        drop' :: Int -> ([Ident], CI.Statement Ann) -> ([Ident], CI.Statement Ann)
        drop' n (args, CI.Return _ (CI.AnonFunction _ args' [body])) | n > 0 = drop' (n-1) (args ++ args', body)
        drop' _ a = a

  declToCpp (CI.Function (_, _, Just ty, _) ident [arg] body) = do
    block <- CppBlock <$> mapM statmentToCpp body
    return $ CppFunction (identToCpp ident)
                         (templparams' $ mktype mn ty)
                         [(identToCpp arg, argtype' mn ty)]
                         (rettype' mn ty)
                         []
                         block

  declToCpp (CI.Function (_, _, Just ty, _) ident args body) = return CppNoOp -- TODO: non-curried functions

  -- |
  -- Typeclass declaration
  --
  declToCpp (CI.Constructor (_, _, _, Just IsTypeClassConstructor) _ (Ident ctor) fields)
    | Just (params, constraints, fns) <- findClass (Qualified (Just mn) (ProperName ctor)) =
    let tmps = runType . Template <$> params
        fnTemplPs = concatMap (templparams' . mktype mn . snd) fns
        classTemplParams = zip tmps $ fromMaybe 0 . flip lookup fnTemplPs <$> tmps
    in
    return $ CppStruct (ctor, Left classTemplParams)
                       (toStrings <$> constraints)
                       (toCpp tmps <$> fns)
                       []
    where
    toCpp :: [String] -> (String, T.Type) -> Cpp
    toCpp tmps (name, ty)
      | ty'@(Just _) <- mktype mn ty,
        atyp@(_:_) <- argtype' mn ty,
        rtyp@(_:_) <- rettype' mn ty
      = CppFunction name
                    (filter (not . (`elem` tmps) . fst) $ templparams' ty')
                    [([], atyp)]
                    rtyp
                    [CppStatic]
                    CppNoOp
    toCpp _ (name, ty) =
      CppVariableIntroduction (name, runQualifier CppStatic ++ ' ' : typestr mn ty) Nothing
    toCpp _ f = error $ show f
    toStrings :: (Qualified ProperName, [T.Type]) -> (String, [String])
    toStrings (name, tys) = (qualifiedToStr' (Ident . runProperName) name, typestr mn <$> tys)

  -- |
  -- data declarations (to omit)
  --
  declToCpp (CI.Constructor (_, _, _, Just IsNewtype) _ ctor _) = return CppNoOp
  declToCpp (CI.Constructor _ _ ctor []) = return CppNoOp
  declToCpp (CI.Constructor (_, _, _, meta) _ ctor fields) = return CppNoOp

  declToCpp d = return CppNoOp -- TODO: includes Function IsNewtype

  statmentToCpp :: CI.Statement Ann -> m Cpp
  statmentToCpp (CI.Expr e) = exprToCpp e
  statmentToCpp (CI.Decl d) = declToCpp d
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
        convert (CppAccessor [] prop cpp) | cpp == acc = CppAccessor qname prop cpp
        convert cpp = cpp
        qname :: String
        qname = qualifiedToStr' (Ident . runProperName) ctor
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
                Just t <- lookup name pairs = CppMapAccessor (CppCast CppNoOp (runType t)) (CppIndexer p e)
    toCpp e p = CppAccessor (maybe [] (typestr mn) ty) name e
  exprToCpp (CI.Accessor _ prop expr) =
    CppIndexer <$> exprToCpp prop <*> exprToCpp expr
  exprToCpp (CI.Indexer _ index expr) =
    CppIndexer <$> exprToCpp index <*> exprToCpp expr
  exprToCpp (CI.AnonFunction (_, _, Just ty, _) [arg] stmnts') = do
    body <- CppBlock <$> mapM statmentToCpp stmnts'
    return $ CppLambda [(identToCpp arg, argtype' mn ty)] (rettype' mn ty) body
  exprToCpp (CI.AnonFunction _ args stmnts') = return CppNoOp -- TODO: non-curried lambdas

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
        let argsNotApp = length fields - length args
            qname = qualifiedToStr' id name
            tmps = maybe [] getDataTypeArgs (mktype mn ty >>= getDataType qname)
            val = CppDataConstructor qname tmps in
        if argsNotApp > 0
          then return (CppPartialApp val args' argsNotApp)
          else return (CppApp val args')

      CI.Var (_, _, _, Just IsTypeClassConstructor) name ->
        return CppNoOp
      CI.Var (_, _, Just ty, _) (Qualified (Just mn') ident) -> do
        let (targs, argsToApply) = if length normalArgs == length args
                                     then (templArgs, args')
                                     else (templArgsFromDicts, filteredArgs)
            targs' = snd <$> targs
        fnToApply <- asTemplate targs' <$> exprToCpp f
        return $ flip (foldl (\fn' a -> CppApp fn' [a])) argsToApply fnToApply
        where
        normalArgs :: [CI.Expr Ann]
        normalArgs = filter (not . isDict) args

        filteredArgs :: [Cpp]
        filteredArgs = catMaybes $ fst <$> templsFromDicts

        templArgsFromDicts :: [(String, String)]
        templArgsFromDicts = nub . sort $ concatMap snd templsFromDicts

        templArgs :: [(String, String)]
        templArgs = nub . sort . concat $ templateArgs <$> tysMapping
          where
          fnTyList = maybe [] (fnTypesN (length normalArgs)) (mktype mn ty)
          exprTyList = (tyFromExpr' <$> normalArgs) ++ [tyFromExpr' e]
          tysMapping = (\(a,b) -> (a, fromJust b)) <$> filter (isJust . snd) (zip fnTyList exprTyList)

        templsFromDicts :: [(Maybe Cpp, [(String, String)])]
        templsFromDicts = go <$> args'
          where
          go :: Cpp -> (Maybe Cpp, [(String, String)])
          go (CppInstance mname (cn, fns) inst params)
            | runModuleName mn' == mname,
              identToCpp ident `elem` fns
              = let paramNames = fst <$> params
                    fp = if any (null . snd) params
                           then zip paramNames . map (fromMaybe "?" . flip lookup templArgs . fst)
                           else id
                    arg' = CppInstance mname (cn, fns) inst (fp params)
                in (Just arg', filter (not . flip elem paramNames . fst) templArgs)
            | otherwise = (Nothing, templArgs)
          go arg@(CppApp (CppIndexer _ inst@CppInstance{}) [CppVar "undefined"]) = go inst
          go arg = (Just arg, [])

      -- TODO: verify this
      _ -> flip (foldl (\fn a -> CppApp fn [a])) args' <$> exprToCpp f

  exprToCpp (CI.Var (_, _, ty, Just (IsConstructor _ fields)) ident) =
    let qname = qualifiedToStr' id ident
        tmps = maybe [] getDataTypeArgs (ty >>= mktype mn >>= getDataType qname) in
    return $ if null fields
               then CppApp (CppDataConstructor (qualifiedToStr' id ident) tmps) []
               else CppPartialApp (CppDataConstructor (qualifiedToStr' id ident) tmps) [] (length fields)

  -- Typeclass instance dictionary
  exprToCpp (CI.Var (_, _, Nothing, Nothing) ident@(Qualified (Just _) (Ident instname)))
    | Just (qname@(Qualified (Just mn') (ProperName cname)), types) <- findInstance ident,
      Just (params, _, fns) <- findClass qname
    = let fs' = fst <$> fns
          params' = runType . Template <$> params
      in return $ CppInstance (runModuleName mn') (cname, fs') instname (zip params' types)
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
          params' = runType . Template <$> params
      in return $ CppInstance mname (cname, fs') [] (zip params' (repeat []))
    where
    getFns :: [T.Constraint] -> [(String, T.Type)]
    getFns = concatMap go
      where
      go :: T.Constraint -> [(String, T.Type)]
      go cls | Just (_, clss, fns) <- findClass (fst cls) = fns ++ getFns clss

  exprToCpp (CI.Var _ ident) =
    return $ varToCpp ident
    where
    varToCpp :: Qualified Ident -> Cpp
    varToCpp (Qualified Nothing ident) = CppVar (identToCpp ident)
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
        val = CppDataType qname tmps in
    flip CppInstanceOf val <$> exprToCpp expr

  modDatasToCpps :: m [Cpp]
  modDatasToCpps
    | ds@(_:_) <- M.toList
                 . M.mapWithKey (\k a -> snd a)
                 . M.filterWithKey (\(Qualified mn' _) _ -> mn' == Just mn)
                 . M.filter (isData . snd)
                 $ types env = do
      let dtys = dataTypes ds
      let dcons = dataCtors ds
      return (dtys ++ dcons)
    | otherwise = return []
    where
      isData :: TypeKind -> Bool
      isData DataType{} = True
      isData _ = False
      dataTypes :: [(Qualified ProperName, TypeKind)] -> [Cpp]
      dataTypes = map go
        where
        go :: (Qualified ProperName, TypeKind) -> Cpp
        go (_, DataType _ [_]) = CppNoOp
        go (typ, DataType ts cs) =
          let name = qual typ in
          CppStruct (name, Left (flip (,) 0 . runType . Template . fst <$> ts))
                    [] []
                    [CppFunction name [] [] [] [CppVirtual, CppDestructor, CppDefault] CppNoOp]
      dataCtors :: [(Qualified ProperName, TypeKind)] -> [Cpp]
      dataCtors = concatMap go
        where
        go :: (Qualified ProperName, TypeKind) -> [Cpp]
        go (typ, DataType ts cs) =
          map ctorStruct cs ++ aliases
          where
          aliases :: [Cpp]
          aliases | [_] <- cs = [CppTypeAlias (qual typ, tmps) ((++ "_") . runProperName . fst $ head cs, tmps)]
                  | otherwise = []
          tmps :: [(String, Int)]
          tmps = flip (,) 0 . runType . Template . fst <$> ts
          ctorStruct :: (ProperName, [T.Type]) -> Cpp
          ctorStruct (ctor, fields) =
            CppStruct (name, Left tmps)
                      supers
                      []
                      members
            where
            name :: String
            name = runProperName ctor ++ "_"
            supers :: [(String, [String])]
            supers | [_] <- cs = []
                   | otherwise = [(qual typ, fst <$> tmps)]
            members :: [Cpp]
            members | ms@(_:_) <- zip (("value" ++) . show <$> [0..]) (typestr mn <$> fields)
                      = (flip CppVariableIntroduction Nothing <$> ms) ++
                        [ CppFunction name [] ms [] [CppConstructor] (CppBlock [])
                        , CppFunction name [] [] [] [CppConstructor, CppDelete] CppNoOp
                        ]
                    | otherwise = []
      qual :: Qualified ProperName -> String
      qual name = qualifiedToStr' (Ident . runProperName) name

  unaryToCpp :: UnaryOp -> CppUnaryOp
  unaryToCpp Negate = CppNegate
  unaryToCpp Not = CppNot
  unaryToCpp BitwiseNot = CppBitwiseNot

  literalToValueCpp :: Literal (CI.Expr Ann) -> m Cpp
  literalToValueCpp (NumericLiteral n) = return $ CppNumericLiteral n
  literalToValueCpp (StringLiteral s) = return $ CppStringLiteral s
  literalToValueCpp (BooleanLiteral b) = return $ CppBooleanLiteral b
  literalToValueCpp (ArrayLiteral xs) = CppArrayLiteral <$> mapM exprToCpp xs
  literalToValueCpp (ObjectLiteral ps) = CppObjectLiteral <$> mapM (sndM exprToCpp) ps

  -- |
  -- Shallow copy an object.
  --
  extendObj :: Cpp -> [(String, Cpp)] -> m Cpp
  extendObj obj sts = do
    newObj <- freshName
    key <- freshName
    let
      cppKey = CppVar key
      cppNewObj = CppVar newObj
      block = CppBlock (objAssign:copy:extend ++ [CppReturn cppNewObj])
      objAssign = CppVariableIntroduction (newObj, []) (Just $ CppObjectLiteral [])
      copy = CppForIn key obj $ CppBlock [CppIfElse cond assign Nothing]
      cond = CppApp (CppAccessor [] "hasOwnProperty" obj) [cppKey]
      assign = CppBlock [CppAssignment (CppIndexer cppKey cppNewObj) (CppIndexer cppKey obj)]
      stToAssign (s, cpp) = CppAssignment (CppAccessor [] s cppNewObj) cpp
      extend = map stToAssign sts
    return $ CppApp (CppLambda [] [] block) []

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
    let tparams' = fst <$> tparams
        block = CppBlock [CppReturn (CppApp (asTemplate tparams' e') [CppVar "arg"])]
    return $ CppFunction name
                         tparams
                         [("arg", argtype' mn ty)]
                         (rettype' mn ty)
                         qs
                         block

  -- |
  -- Generate code in the simplified C++11 intermediate representation for a reference to a
  -- variable that may have a qualified name.
  --
  qualifiedToCpp :: (a -> Ident) -> Qualified a -> Cpp
  qualifiedToCpp f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = CppVar . runIdent $ f a
  qualifiedToCpp f (Qualified (Just mn') a) | mn /= mn' = CppAccessor [] (identToCpp $ f a) (CppScope (moduleNameToCpp mn'))
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
  isDict v = False

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

  asTemplate :: [String] -> Cpp -> Cpp
  asTemplate [] cpp = cpp
  asTemplate ps (CppVar name) = CppVar (name ++ '<' : intercalate "," ps ++ ">")
  asTemplate ps (CppAccessor typ prop cpp)
    | prop' <- P.prettyPrintCpp [asTemplate ps (CppVar prop)] = CppAccessor typ prop' cpp
  asTemplate ps (CppApp f args) = CppApp (asTemplate ps f) args
  asTemplate _ cpp = cpp

  -- |
  -- Find a type class instance in scope by name, retrieving its class name and construction types.
  --
  findInstance :: Qualified Ident -> Maybe (Qualified ProperName, [String])
  findInstance ident@(Qualified (Just mn') _)
    | Just dict <- M.lookup (ident, Just mn) (typeClassDictionaries env),
      classname <- TCD.tcdClassName dict,
      tys <- typestr mn' <$> TCD.tcdInstanceTypes dict
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
