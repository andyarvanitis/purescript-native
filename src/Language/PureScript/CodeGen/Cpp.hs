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
-- This module generates code in the simplified C++14 intermediate representation from Purescript code
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp
  ( module AST
  , module Common
  , moduleToCpp
  , P.prettyPrintCpp
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Function (on)
import Data.Traversable (traverse)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad (forM, liftM2, replicateM, when)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Supply.Class
import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.Cpp.AST as AST
import Language.PureScript.CodeGen.Cpp.Common as Common
import Language.PureScript.CodeGen.Cpp.Data
import Language.PureScript.CodeGen.Cpp.File
import Language.PureScript.CodeGen.Cpp.Optimizer
import Language.PureScript.CodeGen.Cpp.Synonyms
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Sugar.TypeClasses (superClassDictionaryNames)
import Language.PureScript.Traversals (sndM)

import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Environment as E
import qualified Language.PureScript.Pretty.Cpp as P
import qualified Language.PureScript.TypeClassDictionaries as TCD
import qualified Language.PureScript.Types as T

-- import Debug.Trace

-- |
-- Generate code in the simplified C++14 intermediate representation for all declarations in a
-- module.
--
---------------------------------------------------------------------------------------------------
moduleToCpp :: forall m. (Applicative m, Monad m, MonadReader Options m, MonadSupply m)
           => E.Environment -> Module Ann -> m [Cpp]
---------------------------------------------------------------------------------------------------
moduleToCpp env (Module _ mn imps _ foreigns decls) = do
  cppImports <- traverse (pure . runModuleName) . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ imps
  let cppImports' = "PureScript" : cppImports
  cppDecls <- concat <$> mapM bindToCpp decls
  optimized <- traverse optimize (concatMap expandSeq cppDecls)
  datas <- datasToCpps env mn
  synonyms <- synonymsToCpp env mn
  let moduleHeader = fileBegin mn "HH"
                  ++ P.linebreak
                  ++ ((\i -> CppInclude i i) <$> cppImports')
                  ++ (if not (null foreigns)
                        then [CppInclude [] (runModuleName mn ++ "_ffi")]
                        else []
                     )
                  ++ P.linebreak
                  ++ headerDefsBegin mn
                  ++ [CppNamespace (runModuleName mn) $
                       (CppUseNamespace <$> cppImports') ++ P.linebreak
                                                         ++ (depSortSynonymsAndData $ synonyms ++ datas)
                                                         ++ toHeader optimized
                                                         ++ toHeaderFns optimized
                     ]
                  ++ P.linebreak
                  ++ headerDefsEnd
                  ++ P.linebreak
                  ++ fileEnd mn "HH"
  let bodyCpps = toBodyDecl optimized ++ toBody optimized
      moduleBody = fileBegin mn "CC"
                ++ P.linebreak
                ++ CppInclude (runModuleName mn) (runModuleName mn) : P.linebreak
                ++ (if null bodyCpps
                      then []
                      else [CppNamespace (runModuleName mn) $
                             (CppUseNamespace <$> cppImports') ++ P.linebreak ++ bodyCpps])
                ++ P.linebreak
                ++ (if isMain mn then [nativeMain] else [])
                ++ fileEnd mn "CC"
  return $ moduleHeader ++ CppEndOfHeader : moduleBody

  where
  -- |
  -- Generate code in the simplified C++14 intermediate representation for a declaration
  --
  -------------------------------------------------------------------------------------------------
  bindToCpp :: Bind Ann -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  bindToCpp (NonRec ident val) = return <$> declToCpp ident val
  bindToCpp (Rec vals) = forM vals (uncurry declToCpp)

  -- |
  -- Desugar a declaration into a variable introduction or named function
  -- declaration.
  -------------------------------------------------------------------------------------------------
  declToCpp :: Ident -> Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  declToCpp ident (Abs ann@(_, _, _, Just IsTypeClassConstructor) _ _) =
    return CppNoOp

  declToCpp _ (Abs (_, _, _, Just IsNewtype) _ _) =
    return CppNoOp

  declToCpp ident (Abs _ arg@(Ident "dict") body@(Accessor _ _ (Var _ (Qualified Nothing (Ident "dict"))))) = do
    block <- asReturnBlock <$> valueToCpp body
    return $ CppFunction (identToCpp ident)
                         []
                         [(identToCpp arg, Just AnyType)]
                         (Just $ Native ("const " ++ runType AnyType ++ "&") [])
                         [CppInline]
                         block

  declToCpp ident (Abs (_, com, _, _) arg body) = do
    block <- asReturnBlock <$> valueToCpp body
    let block' = convertNestedLambdas [] block
        fn' = CppFunction (identToCpp ident) [] [(identToCpp arg, Just AnyType)] (Just AnyType) [] block'
    return (CppComment com fn')

  declToCpp _ (Constructor {}) =
    return CppNoOp

  -- declToCpp ident val@(App ann@(_, _, Just ty, _) _ _) | Just EffectFunction {} <- mktype mn ty =
  --   mkFunction [] ident ann (Ident "") (App nullAnn val (Var nullAnn (Qualified Nothing (Ident "")))) []

  declToCpp ident val = do
    val' <- valueToCpp val
    return $ CppVariableIntroduction (identToCpp ident, Just AnyType) [] [] (Just val')

  -------------------------------------------------------------------------------------------------
  toLambda :: [CppCaptureType] -> [TemplateInfo] -> Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  toLambda cs encTmplts (CppFunction name _ args rtyp qs body) =
    CppVariableIntroduction (name, Just AnyType)
                            []
                            (filter (==CppStatic) qs)
                            (Just (CppLambda cs' args (Just AnyType) body))
    where
    cs' | (not . null) (qs `intersect` [CppInline, CppStatic]) = []
        | otherwise = cs
  toLambda cs encTmplts (CppComment com cpp) = CppComment com (toLambda cs encTmplts cpp)
  toLambda _ _ _ = error "Not a function!"

  -------------------------------------------------------------------------------------------------
  asReturnBlock :: Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  asReturnBlock cpp = CppBlock [CppReturn cpp]

  -------------------------------------------------------------------------------------------------
  convertNestedLambdas :: [TemplateInfo] -> Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  convertNestedLambdas encTmplts = everywhereOnCpp go
    where
    go :: Cpp -> Cpp
    go f@(CppFunction {}) = toLambda [CppCaptureAll] encTmplts f
    go (CppLambda [] args rtyp body) = CppLambda [CppCaptureAll] args rtyp body
    go cpp = cpp

  -- |
  -- Generate code in the simplified C++14 intermediate representation for a value or expression.
  --
  -------------------------------------------------------------------------------------------------
  valueToCpp :: Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  valueToCpp (Var (_, _, ty, Just (IsConstructor _ fields)) ident) =
    let qname = qualifiedToStr' id ident
        fieldCount = length fields in
    return $ if fieldCount == 0
               then CppApp (CppDataConstructor (qualifiedToStr' id ident) []) []
               else let argTypes = maybe [] (init . fnTypesN fieldCount) (ty >>= mktype mn)
                        argTypes' = replicate (length argTypes) AnyType
                    in
                    CppPartialApp argTypes' fieldCount (CppDataConstructor (qualifiedToStr' id ident) []) []

  valueToCpp (Var (_, _, ty, Just IsNewtype) ident) =
    let qname = qualifiedToStr' id ident
        argTypes = maybe [] (init . fnTypesN 1) (ty >>= mktype mn)
        argTypes' = replicate (length argTypes) AnyType
    in return (CppPartialApp argTypes' 1 (CppDataConstructor (qualifiedToStr' id ident) []) [])

  valueToCpp (Var _ ident) =
    return $ varToCpp ident

  valueToCpp (Literal _ (NumericLiteral n)) =
    return (CppNumericLiteral n)

  valueToCpp (Literal _ (StringLiteral s)) =
    return (CppStringLiteral s)

  valueToCpp (Literal _ (CharLiteral c)) =
    return (CppCharLiteral c)

  valueToCpp (Literal _ (BooleanLiteral b)) =
    return (CppBooleanLiteral b)

  valueToCpp (Literal (_, _, ty, _) (ArrayLiteral xs)) =
    CppArrayLiteral Nothing <$> mapM valueToCpp xs

  valueToCpp (Literal _ (ObjectLiteral ps)) =
    CppObjectLiteral <$> sortBy (compare `on` fst) <$> mapM (sndM valueToCpp) ps

  valueToCpp (Accessor _ prop val) =
    CppIndexer <$> pure (CppStringLiteral prop) <*> valueToCpp val

  -- TODO: use a more efficient way of copying/updating the map
  valueToCpp (ObjectUpdate (_, _, Just ty, _) obj ps)
    | Just (Map allKeys) <- mktype mn ty = do
    obj' <- valueToCpp obj
    updatedFields <- mapM (sndM valueToCpp) ps
    let origKeys = (fst <$> allKeys) \\ (fst <$> updatedFields)
        origFields = (\key -> (key, CppIndexer (CppStringLiteral key) obj')) <$> origKeys
    return $ CppObjectLiteral . sortBy (compare `on` fst) $ origFields ++ updatedFields

  valueToCpp (ObjectUpdate _ _ _) = error $ "Bad Type in object update!"

  valueToCpp (Abs (_, _, ty, _) arg body) = do
    cpp <- valueToCpp body
    let cpp' = convertNestedLambdas [] cpp
    return $ CppLambda [] [(identToCpp arg, Just AnyType)] (Just AnyType) (asReturnBlock cpp')

  -- valueToCpp (App _ e (Var _ (Qualified (Just (ModuleName [ProperName "Prim"])) (Ident "undefined")))) =
  --   valueToCpp e

  valueToCpp e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToCpp args
    case f of
      Var (_, _, Just ty, Just IsNewtype) name ->
        let qname = qualifiedToStr' id name
            val = CppDataConstructor qname [] in
        return (CppApp val args')

      Var (_, _, Just ty, Just (IsConstructor _ fields)) name ->
        let fieldCount = length fields
            argsNotApp = fieldCount - length args
            qname = qualifiedToStr' id name
            val = CppDataConstructor qname [] in
        if argsNotApp > 0
          then let argTypes = maybe [] (init . fnTypesN fieldCount) (mktype mn ty)
                   argTypes' = replicate (length argTypes) AnyType
               in
               return (CppPartialApp argTypes' argsNotApp val args')
          else return (CppApp val args')

      Var (_, _, _, Just IsTypeClassConstructor) (Qualified mn' (Ident classname)) ->
        let Just (params, constraints, fns) = findClass (Qualified mn' (ProperName classname)) in
        return . CppObjectLiteral $ zip (superClassDictionaryNames constraints ++ (fst <$> fns)) args'

      _ -> -- TODO: verify this
        flip (foldl (\fn a -> CppApp fn [a])) args' <$> valueToCpp f

  valueToCpp (Case (maybeSpan, _, ty, _) values binders) = do
    vals <- mapM valueToCpp values
    bindersToCpp maybeSpan ty binders vals

  valueToCpp (Let (_, _, ty, _) ds val) = do
    ds' <- concat <$> mapM bindToCpp ds
    ret <- valueToCpp val
    let ds'' = convertNestedLambdas [] <$> ds'
    return $ CppApp (CppLambda [] [] Nothing (CppBlock (ds'' ++ [CppReturn ret]))) []

  valueToCpp (Constructor {}) =
    return CppNoOp

  -- |
  -- Generate code in the simplified C++14 intermediate representation for a reference to a
  -- variable.
  --
  -------------------------------------------------------------------------------------------------
  varToCpp :: Qualified Ident -> Cpp
  -------------------------------------------------------------------------------------------------
  varToCpp (Qualified Nothing ident) = CppVar (identToCpp ident)
  varToCpp qual = qualifiedToCpp id qual

  -- |
  -- Generate code in the simplified C++14 intermediate representation for pattern match binders
  -- and guards.
  --
  -------------------------------------------------------------------------------------------------
  bindersToCpp :: Maybe SourceSpan -> Maybe T.Type -> [CaseAlternative Ann] -> [Cpp] -> m Cpp
  -------------------------------------------------------------------------------------------------
  bindersToCpp maybeSpan ty binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith mkVarDecl valNames (map Just vals)
    cpps <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToCpp result
      go valNames ret bs
    return $ CppApp (CppLambda []
                               []
                               (Just AnyType)
                               (CppBlock (assignments ++ concat cpps ++ [failedPatternError valNames]))) []
    where
      mkVarDecl :: String -> Maybe Cpp -> Cpp
      mkVarDecl name = CppVariableIntroduction (name, Nothing) [] []
      go :: [String] -> [Cpp] -> [Binder Ann] -> m [Cpp]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToCpp v done'' b
      go _ _ _ = error "Invalid arguments to bindersToCpp"

      failedPatternError :: [String] -> Cpp
      failedPatternError _ =
        CppThrow $ CppApp (CppVar "runtime_error") [CppStringLiteral errorMessage]

      errorMessage :: String
      errorMessage = "Failed pattern match" ++ maybe "" (((" at " ++ runModuleName mn ++ " ") ++) . displayStartEndPos) maybeSpan ++ ": "

      guardsToCpp :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [Cpp]
      guardsToCpp (Left gs) = forM gs $ \(cond, val) -> do
        cond' <- valueToCpp cond
        done  <- valueToCpp val
        return $ CppIfElse cond' (asReturnBlock done) Nothing
      guardsToCpp (Right v) = return . CppReturn <$> valueToCpp v

  -- |
  -- Generate code in the simplified C++14 intermediate representation for a pattern match
  -- binder.
  --
  -------------------------------------------------------------------------------------------------
  binderToCpp :: String -> [Cpp] -> Binder Ann -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  binderToCpp _ done (NullBinder{}) = return done

  binderToCpp varName done (LiteralBinder _ l) =
    literalToBinderCpp varName done l

  binderToCpp varName done (VarBinder (_, _, ty, _) ident) =
    return (CppVariableIntroduction (identToCpp ident, Just AnyType) [] [] (Just (CppVar varName)) : done)

  binderToCpp varName done (ConstructorBinder (com, ss, ty, Just IsNewtype) c ctor bs) =
    binderToCpp varName done (ConstructorBinder (com, ss, ty, Just (IsConstructor ProductType [Ident "0"])) c ctor bs)

  binderToCpp varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
    cpps <- go (zip fields bs) done
    let ctor' = qualifiedToStr' (Ident . runProperName) ctor
    cpps' <- forM cpps (addTypes (CppVar varName))
    return $ case ctorType of
      ProductType -> cpps'
      SumType ->
        [ CppIfElse (CppInstanceOf (CppVar varName) (CppData ctor' []))
                    (CppBlock cpps')
                    Nothing ]
    where
    go :: [(Ident, Binder Ann)] -> [Cpp] -> m [Cpp]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      cpp <- binderToCpp argVar done'' binder
      return (CppVariableIntroduction (argVar, Nothing)
                                      []
                                      []
                                      (Just (CppAccessor Nothing (CppVar (identToCpp field)) (CppVar varName)))
              : cpp)
    addTypes :: Monad m => Cpp -> Cpp -> m Cpp
    addTypes cpp1 cpp2 = do
      return (typeAccessors cpp1 cpp2)
      where
      typeAccessors :: Cpp -> Cpp -> Cpp
      typeAccessors acc = everywhereOnCpp convert
        where
        convert :: Cpp -> Cpp
        convert (CppAccessor Nothing prop cpp) | cpp == acc = CppAccessor qtyp prop cpp
        convert cpp = cpp
        qtyp :: Maybe Type
        qtyp = Just (Native (qualifiedToStr' (Ident . runProperName) ctor) [])

  binderToCpp _ _ b@(ConstructorBinder{}) =
    error $ "Invalid ConstructorBinder in binderToCpp: " ++ show b

  binderToCpp varName done (NamedBinder (_, _, ty, _) ident binder) = do
    cpp <- binderToCpp varName done binder
    return (CppVariableIntroduction (identToCpp ident, Just AnyType) [] [] (Just (CppVar varName)) : cpp)

  -------------------------------------------------------------------------------------------------
  literalToBinderCpp :: String -> [Cpp] -> Literal (Binder Ann) -> m [Cpp]
  -------------------------------------------------------------------------------------------------
  literalToBinderCpp varName done (NumericLiteral num) =
    return [CppIfElse (CppBinary Equal (CppVar varName) (CppNumericLiteral num)) (CppBlock done) Nothing]

  literalToBinderCpp varName done (CharLiteral c) =
    return [CppIfElse (CppBinary Equal (CppVar varName) (CppCharLiteral c)) (CppBlock done) Nothing]

  literalToBinderCpp varName done (StringLiteral str) =
    return [CppIfElse (CppBinary Equal (CppVar varName) (CppStringLiteral str)) (CppBlock done) Nothing]

  literalToBinderCpp varName done (BooleanLiteral True) =
    return [CppIfElse (CppCast (fromJust $ mktype mn E.tyBoolean) (CppVar varName)) (CppBlock done) Nothing]

  literalToBinderCpp varName done (BooleanLiteral False) =
    return [CppIfElse (CppUnary CppNot (CppCast (fromJust $ mktype mn E.tyBoolean) (CppVar varName))) (CppBlock done) Nothing]

  literalToBinderCpp varName done (ObjectLiteral bs) = go done bs
    where
    go :: [Cpp] -> [(String, Binder Ann)] -> m [Cpp]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      cpp <- binderToCpp propVar done'' binder
      return (CppVariableIntroduction (propVar, Nothing)
                                      []
                                      []
                                      (Just (CppAccessor Nothing (CppStringLiteral prop) (CppVar varName)))
              : cpp)

  literalToBinderCpp varName done (ArrayLiteral bs) = do
    cpp <- go done 0 bs
    let cond = case length bs of
                 0 -> CppBinary Dot (CppVar varName) (CppApp (CppVar "empty") [])
                 n -> CppBinary Equal (CppBinary Dot (CppVar varName) (CppApp (CppVar "size") []))
                                      (CppNumericLiteral (Left (fromIntegral n)))
    return [ CppIfElse cond (CppBlock cpp) Nothing ]
    where
    go :: [Cpp] -> Integer -> [Binder Ann] -> m [Cpp]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      cpp <- binderToCpp elVar done'' binder
      return (CppVariableIntroduction (elVar, Nothing) [] []
                                      (Just (CppIndexer (CppNumericLiteral (Left index)) (CppVar varName))) : cpp)

  -------------------------------------------------------------------------------------------------

  unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
  unApp (App _ val arg) args = unApp val (arg : args)
  unApp other args = (other, args)

  qualifiedToCpp :: (a -> Ident) -> Qualified a -> Cpp
  qualifiedToCpp f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = CppVar . runIdent $ f a
  qualifiedToCpp f (Qualified (Just mn') a)
    | mn /= mn' = CppAccessor Nothing (CppVar . identToCpp $ f a) (CppScope (moduleNameToCpp mn'))
  qualifiedToCpp f (Qualified _ a) = CppVar $ identToCpp (f a)

  qualifiedToStr' :: (a -> Ident) -> Qualified a -> String
  qualifiedToStr' = qualifiedToStr mn

  -- |
  -- Find a class in scope by name, retrieving its list of constraints, function names and types.
  --
  findClass :: Qualified ProperName -> Maybe ([String], [T.Constraint], [(String, T.Type)])
  findClass name
    | Just (params, fns, constraints) <- M.lookup name (E.typeClasses env),
      fns' <- (\(i,t) -> (identToCpp i, t)) <$> fns
      = Just (fst <$> params, (sortBy (compare `on` fst) constraints), (sortBy (compare `on` normalizedName . fst) fns'))
  findClass _ = Nothing

  -- |
  -- Find a value (incl functions) in scope by name, retrieving its type
  --
  findValue :: ModuleName -> Ident -> Maybe T.Type
  findValue mname ident
    | Just (ty, _, _) <- M.lookup (mname, ident) (E.names env) = Just ty
  findValue _ _ = Nothing
