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

import Data.List ((\\), delete, sortBy)
import Data.Function (on)
import Data.Maybe (mapMaybe, fromMaybe)
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
  let foreigns' = mapMaybe (\(_, cpp, _) -> CppRaw . runForeignCode <$> cpp) foreigns
  cppDecls <- mapM declToCpp decls
  optimized <- T.traverse optimize (concatMap expandSeq cppDecls)
  let isModuleEmpty = null exps
  comments <- not <$> asks optionsNoComments
  let header = if comments && not (null coms) then CppComment coms CppNoOp else CppNoOp
  let moduleBody = header : (CppInclude <$> cppImports')
                   ++ [CppNamespace (runModuleName mn) $
                        (CppUseNamespace "PureScript") : foreigns' ++ optimized]
  return $ case additional of
    MakeOptions -> moduleBody
    CompileOptions _ _ _ | not isModuleEmpty -> moduleBody
    _ -> []

  where
  -- |
  -- Generates C++11 code for a variable reference based on a PureScript
  -- identifier. The ident will be mangled if necessary to produce a valid Cpp
  -- identifier.
  --
  var :: Ident -> Cpp
  var = CppVar . identToCpp

  declToCpp :: CI.Decl Ann -> m Cpp
  -- |
  -- Typeclass instance definition
  --
  declToCpp (CI.VarDecl _ ident expr)
    | Just (classname, typs) <- findInstance (Qualified (Just mn) ident),
      Just (_, _, fns) <- findClass classname = do
    let (_, fs) = unApp expr []
    let classname' = qualifiedToStr mn (Ident . runProperName) classname
    let inst = CppInstance [] classname' [] typs
    cpps <- mapM toFn (zip fns fs)
    return $ CppStruct (classname', Right typs) [] cpps []
    where
    toFn :: ((Ident, T.Type), CI.Expr Ann) -> m Cpp
    toFn ((name, _), CI.AnonFunction ty ags sts) = do
      fn' <- declToCpp $ CI.Function ty name ags sts
      return (addQual CppStatic fn')
    toFn ((Op s, ty), e) = toFn ((Ident $ identToCpp (Ident s), ty), e)
    toFn i = return CppNoOp
    addQual :: CppQualifier -> Cpp -> Cpp
    addQual q (CppFunction name tmps args rty qs cpp) = CppFunction name tmps args rty (q : qs) cpp
    addQual _ cpp = cpp
  declToCpp (CI.VarDecl _ ident expr) =
    CppVariableIntroduction (identToCpp ident) . Just <$> exprToCpp expr

  declToCpp (CI.Function _ ident [Ident "dict"]
    [CI.Return _ (CI.Accessor _ _ (CI.Var _ (Qualified Nothing (Ident "dict"))))]) =
    return CppNoOp

  declToCpp (CI.Function (_, _, Just ty, _) ident [arg] body) = do
    block <- CppBlock <$> mapM statmentToCpp body
    return $ CppFunction (identToCpp ident)
                         (templparams' $ mktype mn ty)
                         [(identToCpp arg, argtype' mn ty)]
                         (rettype' mn ty)
                         []
                         block
  declToCpp (CI.Function (_, _, Just ty, _) ident args body) = return CppNoOp -- TODO: non-curried functions

  declToCpp (CI.Constructor (_, _, _, Just IsNewtype) _ ctor _) =
    return CppNoOp
  declToCpp (CI.Constructor _ _ ctor []) =
    return CppNoOp
  -- |
  -- Typeclass declaration
  --
  declToCpp (CI.Constructor (_, _, _, Just IsTypeClassConstructor) _ (Ident ctor) fields)
    | Just (params, constraints, fns) <- findClass (Qualified (Just mn) (ProperName ctor)) =
    let tmps = runType . Template <$> params in
    return $ CppStruct (ctor, Left params)
                       (toStrings <$> constraints)
                       (toFn tmps <$> fns)
                       []
    where
    toFn :: [String] -> (Ident, T.Type) -> Cpp
    toFn tmps (Ident name, ty) = CppFunction name
                                             (templparams' (mktype mn ty) \\ tmps)
                                             [([], argtype' mn ty)]
                                             (rettype' mn ty)
                                             [CppStatic]
                                             CppNoOp
    toFn tmps (Op s, ty) = toFn tmps (Ident $ identToCpp (Ident s), ty)
    toFn _ f = error $ show f
    toStrings :: (Qualified ProperName, [T.Type]) -> (String, [String])
    toStrings (name, tys) = (qualifiedToStr mn (Ident . runProperName) name, typestr mn <$> tys)

  declToCpp (CI.Constructor (_, _, _, meta) _ ctor fields) =
    return CppNoOp

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
    thens' <- CppBlock <$> mapM statmentToCpp thens
    CppIfElse <$> exprToCpp cond <*> pure thens' <*> pure Nothing
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
  exprToCpp (CI.Accessor _ prop expr) =
    CppIndexer <$> exprToCpp prop <*> exprToCpp expr
  exprToCpp (CI.Indexer _ index expr) =
    CppIndexer <$> exprToCpp index <*> exprToCpp expr
  exprToCpp (CI.AnonFunction (_, _, Just ty, _) [arg] stmnts') = do
    body <- CppBlock <$> mapM statmentToCpp stmnts'
    return $ CppLambda [(identToCpp arg, argtype' mn ty)] (rettype' mn ty) body
  exprToCpp (CI.AnonFunction _ args stmnts') = return CppNoOp -- TODO: non-curried lambdas

  exprToCpp (CI.App _ f []) = flip CppApp [] <$> exprToCpp f
  exprToCpp e@CI.App{} = do
    let (f, args) = unApp e []
    args' <- mapM exprToCpp args
    case f of
      CI.Var (_, _, _, Just IsNewtype) _ -> return (head args')
      CI.Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ CppUnary CppNew $ CppApp (qualifiedToCpp id name) args'

      -- TODO: remove after confirming no longer needed
      CI.Var (_, _, _, Just IsTypeClassConstructor) name ->
        return CppNoOp

      _ -> flip (foldl (\fn a -> CppApp fn [a])) args' <$> exprToCpp f

  exprToCpp (CI.Var (_, _, _, Just (IsConstructor _ [])) ident) =
    return $ CppAccessor "value" $ qualifiedToCpp id ident
  exprToCpp (CI.Var (_, _, _, Just (IsConstructor _ _)) ident) =
    return $ CppAccessor "create" $ qualifiedToCpp id ident
  exprToCpp (CI.Var (_, _, Nothing, Nothing) ident@(Qualified (Just _) (Ident instname)))
    | Just (Qualified (Just mn') (ProperName classname), types) <- findInstance ident
    = return $ CppInstance (modname mn') classname instname types
      where
      modname m | m == mn = []
      modname m = runModuleName m
  exprToCpp (CI.Var _ ident) =
    return $ varToCpp ident
  exprToCpp (CI.ObjectUpdate _ obj ps) = do
    obj' <- exprToCpp obj
    ps' <- mapM (sndM exprToCpp) ps
    extendObj obj' ps'
  exprToCpp (CI.UnaryOp _ op expr) =
    CppUnary (unaryToCpp op) <$> exprToCpp expr
  exprToCpp (CI.BinaryOp _ op lhs rhs) =
    CppBinary op <$> exprToCpp lhs <*> exprToCpp rhs
  exprToCpp (CI.IsTagOf _ ctor expr) =
    flip CppInstanceOf (qualifiedToCpp (Ident . runProperName) ctor) <$> exprToCpp expr

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
      objAssign = CppVariableIntroduction newObj (Just $ CppObjectLiteral [])
      copy = CppForIn key obj $ CppBlock [CppIfElse cond assign Nothing]
      cond = CppApp (CppAccessor "hasOwnProperty" obj) [cppKey]
      assign = CppBlock [CppAssignment (CppIndexer cppKey cppNewObj) (CppIndexer cppKey obj)]
      stToAssign (s, cpp) = CppAssignment (CppAccessor s cppNewObj) cpp
      extend = map stToAssign sts
    return $ CppApp (CppLambda [] [] block) []

  -- |
  --
  unApp :: CI.Expr Ann -> [CI.Expr Ann] -> (CI.Expr Ann, [CI.Expr Ann])
  unApp (CI.App _ val args1) args2 = unApp val (args1 ++ args2)
  unApp other args = (other, args)

  -- |
  -- Generate code in the simplified C++11 intermediate representation for a reference to a
  -- variable.
  --
  varToCpp :: Qualified Ident -> Cpp
  varToCpp (Qualified Nothing ident) = var ident
  varToCpp qual = qualifiedToCpp id qual

  -- |
  -- Generate code in the simplified C++11 intermediate representation for a reference to a
  -- variable that may have a qualified name.
  --
  qualifiedToCpp :: (a -> Ident) -> Qualified a -> Cpp
  qualifiedToCpp f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = CppVar . runIdent $ f a
  qualifiedToCpp f (Qualified (Just mn') a) | mn /= mn' = CppAccessor (identToCpp $ f a) (CppScope (moduleNameToCpp mn'))
  qualifiedToCpp f (Qualified _ a) = CppVar $ identToCpp (f a)

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
  findClass :: Qualified ProperName -> Maybe ([String], [T.Constraint], [(Ident, T.Type)])
  findClass name
    | Just (params, fns, constraints) <- M.lookup name (typeClasses env)
      = Just (fst <$> params, constraints, (sortBy (compare `on` fst) fns))
  findClass _ = Nothing
