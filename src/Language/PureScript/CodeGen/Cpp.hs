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

module Language.PureScript.CodeGen.Cpp (
    module AST,
    module Common,
    moduleToCpp
) where

import Data.List ((\\), delete)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Traversable as T (traverse)
import qualified Data.Map as M

import Control.Applicative
import Control.Arrow ((&&&))
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
  optimized <- T.traverse optimize cppDecls
  let isModuleEmpty = null exps
  comments <- not <$> asks optionsNoComments
  let header = if comments && not (null coms) then CppComment coms CppNoOp else CppNoOp
  let moduleBody = header : (CppInclude <$> cppImports')
                   ++ [CppNamespace (runModuleName mn) $
                        (CppUseNamespace <$> cppImports') ++ foreigns' ++ optimized]
  return $ case additional of
    MakeOptions -> moduleBody
    CompileOptions ns _ _ | not isModuleEmpty -> moduleBody
    _ -> []

  where
  -- |
  -- Generates C++11 code for a variable reference based on a PureScript
  -- identifier. The ident will be mangled if necessary to produce a valid Cpp
  -- identifier.
  --
  var :: Ident -> Cpp
  var = CppVar . identToCpp

  -- |
  -- Generate C++11 for an accessor based on a PureScript identifier. If
  -- the name is not valid in C++11 (symbol based, reserved name) an
  -- indexer is returned.
  --
  accessor :: Ident -> Cpp -> Cpp
  accessor (Ident prop) = accessorString prop
  accessor (Op op) = CppIndexer (CppStringLiteral op)

  accessorString :: String -> Cpp -> Cpp
  accessorString prop | identNeedsEscaping prop = CppIndexer (CppStringLiteral prop)
                      | otherwise = CppAccessor prop

  declToCpp :: CI.Decl Ann -> m Cpp
  declToCpp (CI.VarDecl _ ident expr) =
    CppVariableIntroduction (identToCpp ident) . Just <$> exprToCpp expr

  declToCpp (CI.Function _ ident arg body) = do
    let (args', body') = expand body
    block <- CppBlock <$> mapM statmentToCpp body'
    return $ CppFunction (identToCpp ident) (identToCpp <$> arg ++ args') block
    where
    expand ((CI.Return _ (CI.AnonFunction _ ags sts)) : bs) = (ags ++ (fst $ expand sts), (snd $ expand sts) ++ bs)
    expand d = ([], d)

  declToCpp (CI.Constructor (_, _, _, Just IsNewtype) _ ctor _) =
    return $ CppVariableIntroduction (identToCpp ctor) (Just $
                CppObjectLiteral [("create",
                  CppLambda ["value"]
                    (CppBlock [CppReturn $ CppVar "value"]))])
  declToCpp (CI.Constructor _ _ ctor []) =
    let ctor' = identToCpp ctor
    in return $ iifeDecl ctor' [ CppFunction ctor' [] (CppBlock [])
                               , CppAssignment (CppAccessor "value" (var ctor))
                                              (CppUnary CppNew $ CppApp (var ctor) []) ]
  declToCpp (CI.Constructor (_, _, _, meta) _ ctor fields) =
    let constructor =
          let body = [ CppAssignment (accessor f (CppVar "this")) (var f) | f <- fields ]
          in CppFunction (identToCpp ctor) (identToCpp `map` fields) (CppBlock body)
        createFn =
          let body = CppUnary CppNew $ CppApp (var ctor) (var `map` fields)
          in foldr (\f inner -> CppLambda [identToCpp f] (CppBlock [CppReturn inner])) body fields
    in return $
      if meta == Just IsTypeClassConstructor
      then constructor
      else iifeDecl (identToCpp ctor) [ constructor
                                     , CppAssignment (CppAccessor "create" (var ctor)) createFn
                                     ]

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
    return . CppThrow . CppUnary CppNew $ CppApp (CppVar "Error") [CppStringLiteral msg]
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
  exprToCpp (CI.AnonFunction _ args stmnts') = do
    body <- CppBlock <$> mapM statmentToCpp stmnts'
    return $ CppLambda (identToCpp `map` args) body
  exprToCpp (CI.App _ f []) = flip CppApp [] <$> exprToCpp f
  exprToCpp e@CI.App{} = do
    let (f, args) = unApp e []
    args' <- mapM exprToCpp args
    case f of
      CI.Var (_, _, _, Just IsNewtype) _ -> return (head args')
      CI.Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ CppUnary CppNew $ CppApp (qualifiedToCpp id name) args'
      CI.Var (_, _, _, Just IsTypeClassConstructor) name ->
        return $ CppUnary CppNew $ CppApp (qualifiedToCpp id name) args'
      _ -> do
        f' <- exprToCpp f
        let arity' = arity $ getType f
        return $ if maybe False (numNonDict args <) arity' then
                   CppPartialApp f' args'
                 else
                   CppApp f' args'
    where
    unApp :: CI.Expr Ann -> [CI.Expr Ann] -> (CI.Expr Ann, [CI.Expr Ann])
    unApp (CI.App _ val args1) args2 = unApp val (args1 ++ args2)
    unApp other args = (other, args)
    getType ::  CI.Expr Ann -> Maybe Type
    getType (CI.Var (_, _, Just ty', _) _) = mktype mn ty'
    getType _ = Nothing
    numNonDict :: [CI.Expr Ann] -> Int
    numNonDict = length . filter (not . isDict)
    isDict :: CI.Expr Ann -> Bool
    isDict (CI.Var (_, _, Nothing, Nothing) _) = True
    isDict _ = False
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

  iife :: String -> [Cpp] -> Cpp
  iife v exprs = CppApp (CppLambda [] (CppBlock $ exprs ++ [CppReturn $ CppVar v])) []

  iifeDecl :: String -> [Cpp] -> Cpp
  iifeDecl v exprs = CppVariableIntroduction v (Just $ iife v exprs)

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
    return $ CppApp (CppLambda [] block) []

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
  qualifiedToCpp f (Qualified (Just mn') a) | mn /= mn' = accessor (f a) (CppScope (moduleNameToCpp mn'))
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
