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
import Language.PureScript.CodeGen.Cpp.Templates
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Options
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
  let bodyCpps = toBody optimized
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

  -- Desugar a declaration into a variable introduction or named function
  -- declaration.
  -------------------------------------------------------------------------------------------------
  declToCpp :: Ident -> Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  declToCpp ident (Abs ann@(_, _, _, Just IsTypeClassConstructor) _ _) =
    mkTypeClass ident ann

  declToCpp ident (Literal ann@(_, _, _, Just IsTypeClassConstructor) (ObjectLiteral [])) =
    mkTypeClass ident ann

  declToCpp _ (Abs (_, _, _, Just IsNewtype) _ _) =
    return CppNoOp

  -- Typeclass instance definitions
  --
  declToCpp ident val | Just _ <- findInstance (Qualified (Just mn) ident) =
    case val of
      Abs (_, _, Just T.ConstrainedType{}, _) _ expr -> mkInstance ident expr
      _ -> mkInstance ident val

  -- TODO: fix when appropriate Meta info added
  declToCpp _ (Abs _ (Ident "dict") (Accessor _ _ (Var _ (Qualified Nothing (Ident "dict"))))) =
    return CppNoOp

  declToCpp ident (Abs ann arg body) =
    mkFunction [] ident ann arg body []

  declToCpp _ (Constructor {}) =
    return CppNoOp

  declToCpp ident val@(App ann@(_, _, Just ty, _) _ _) | Just EffectFunction {} <- mktype mn ty =
    mkFunction [] ident ann (Ident "") (App nullAnn val (Var nullAnn (Qualified Nothing (Ident "")))) []

  -- convert templated values to functions
  declToCpp ident val@(Var ann@(_, _, Just ty, _) _) | typ <- mktype mn ty,
                                                       (_:_) <- templparams' typ
                                                     = asFunction [] ident ann val [CppInline]

  declToCpp ident val@(App ann@(_, _, Just ty, _) _ _) | typ <- mktype mn ty,
                                                         (_:_) <- templparams' typ
                                                       = asFunction [] ident ann val [CppInline]
  declToCpp ident val = do
    let typ = typFromExpr val
        tmplts = templparams' typ
    val' <- valueToCpp val
    let cpp' = nestedFnsToLambdas tmplts val'
    return $ CppVariableIntroduction (identToCpp ident, typFromExpr val) tmplts [] (Just cpp')

  -------------------------------------------------------------------------------------------------
  mkFunction :: [TemplateInfo] -> Ident -> Ann -> Ident -> Expr Ann -> [CppQualifier] -> m Cpp
  -------------------------------------------------------------------------------------------------
  -- Strip any contraint dict param from function decl
  mkFunction encTmplts ident ann@(_, _, Just T.ConstrainedType {}, _) arg (Abs _ arg' body') qs
    | "__dict_" `isPrefixOf` runIdent arg = mkFunction encTmplts ident ann arg' body' qs

  -- Point-free instance functions
  mkFunction encTmplts ident ann@(_, _, Just ty, _) arg body qs
    | "__dict_" `isPrefixOf` runIdent arg, -- TODO: would prefer a less fragile way
      (_:_) <- templparams' (mktype mn ty) = do
      asFunction encTmplts ident ann body qs

  -- This covers 'let' expressions
  mkFunction _ ident (_, _, Nothing, _) arg body _ = do
    block <- asReturnBlock <$> valueToCpp body
    return $ CppVariableIntroduction (identToCpp ident, Nothing)
                                     []
                                     []
                                     (Just (CppLambda [CppCaptureAll]
                                                      [(identToCpp arg, Just AutoType)]
                                                      Nothing
                                                      block))

  mkFunction encTmplts ident (_, com, ty, _) arg body qs = do
    let typ = ty >>= mktype mn
        tmplts = tmpltsReplFromRight (templparams' typ) (filter isParameterized $ templparams' typ)
    block <- asReturnBlock <$> valueToCpp body
    let block' = nestedFnsToLambdas (nub $ encTmplts ++ tmplts) block
        atyp = argtype typ
        rtyp = rettype typ
        arg' = if null (runIdent arg) then [] else [(identToCpp arg, Just $ fromMaybe AutoType atyp)]
        fn = CppFunction (identToCpp ident) (tmplts \\ encTmplts) arg' rtyp qs block'
        fn' | atyp == Just AutoType = toLambda [CppCaptureAll] [] fn
            | otherwise = fn
    return (CppComment com fn')

  -------------------------------------------------------------------------------------------------
  asFunction :: [TemplateInfo] -> Ident -> Ann -> Expr Ann -> [CppQualifier] -> m Cpp
  -------------------------------------------------------------------------------------------------
  asFunction encTmplts ident ann@(_, _, ty, _) e qs = do
    arg' <- freshName
    mkFunction encTmplts ident ann (Ident arg') (toApp (Ident arg')) qs
      where
      toApp :: Ident -> Expr Ann
      toApp arg | (Var _ qid@(Qualified mn' vid)) <- e,
                  Just ty' <- findValue (fromMaybe mn mn') vid
                  = App (Nothing, [], ty, Nothing)
                        (Var (Nothing, [], Just ty', Nothing) qid)
                        (Var nullAnn (Qualified Nothing arg))
                | (Var ann'@(_, _, Just _, _) qid) <- e
                  = App (Nothing, [], ty, Nothing)
                        (Var ann' qid)
                        (Var nullAnn (Qualified Nothing arg))
                | otherwise
                  = App ann e (Var nullAnn (Qualified Nothing arg))

  -------------------------------------------------------------------------------------------------
  toLambda :: [CppCaptureType] -> [TemplateInfo] -> Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  toLambda cs encTmplts (CppFunction name tmplts args rtyp qs body) =
    let tmplts' = tmplts \\ encTmplts in
    case (cs, tmplts') of
      ((_:_), (_:_)) -> error $ "Rank-N types not supported in C++ backend (unknown line number, "
                              ++ name ++ " :: forall "
                              ++ intercalate " " ((\s -> toLower <$> s) . fst <$> tmplts) ++ ". ...)\n"
      _ -> CppVariableIntroduction (name, ftyp)
                                   tmplts'
                                   (filter (==CppStatic) qs)
                                   (Just (CppLambda cs' args rtyp body))
    where
    cs' | (not . null) (qs `intersect` [CppInline, CppStatic]) = []
        | otherwise = cs

    ftyp = do arg <- listToMaybe args
              atyp' <- snd arg
              rtyp' <- rtyp
              return $ if atyp' == AutoType || rtyp' == AutoType
                         then AutoType
                         else Function atyp' rtyp'

  toLambda cs encTmplts (CppComment com cpp) = CppComment com (toLambda cs encTmplts cpp)
  toLambda _ _ _ = error "Not a function!"

  -------------------------------------------------------------------------------------------------
  asReturnBlock :: Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  asReturnBlock cpp = CppBlock [CppReturn cpp]

  -------------------------------------------------------------------------------------------------
  nestedFnsToLambdas :: [TemplateInfo] -> Cpp -> Cpp
  -------------------------------------------------------------------------------------------------
  nestedFnsToLambdas encTmplts = everywhereOnCpp go
    where
    go :: Cpp -> Cpp
    go f@(CppFunction {}) = toLambda [CppCaptureAll] encTmplts f
    go cpp = cpp

  -- |
  -- Generate code in the simplified C++14 intermediate representation for a value or expression.
  --
  -------------------------------------------------------------------------------------------------
  valueToCpp :: Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
 -- Constraint dictionary
 -- TODO: Make sure this pattern will not change in PS
  valueToCpp (Var (_, _, Nothing, Nothing) (Qualified Nothing (Ident name)))
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

  -- Typeclass instance dictionary
  valueToCpp (Var (_, _, Nothing, Nothing) ident@(Qualified (Just _) (Ident instname)))
    | Just (qname@(Qualified (Just mn') (ProperName cname)), types') <- findInstance ident,
      Just (params, _, fns) <- findClass qname
    = let fs' = (\(f,t) -> (f, mktype mn t)) <$> fns
      in return $ CppInstance (runModuleName mn') ([cname], fs') instname (zip params types')

  valueToCpp (Var (_, _, ty, Just (IsConstructor _ fields)) ident) =
    let qname = qualifiedToStr' id ident
        tmplts = maybe [] getDataTypeArgs (ty >>= mktype mn >>= getDataType qname)
        fieldCount = length fields in
    return $ if fieldCount == 0
               then CppApp (CppDataConstructor (qualifiedToStr' id ident) tmplts) []
               else let argTypes = maybe [] (init . fnTypesN fieldCount) (ty >>= mktype mn) in
                    CppPartialApp (CppDataConstructor (qualifiedToStr' id ident) tmplts) [] argTypes fieldCount

  valueToCpp (Var (_, _, ty, Just IsNewtype) ident) =
    let qname = qualifiedToStr' id ident
        tmplts = maybe [] getDataTypeArgs (ty >>= mktype mn >>= getDataType qname)
        argTypes = maybe [] (init . fnTypesN 1) (ty >>= mktype mn)
    in return (CppPartialApp (CppDataConstructor (qualifiedToStr' id ident) tmplts) [] argTypes 1)

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
    CppArrayLiteral (ty >>= mktype mn) <$> mapM valueToCpp xs

  valueToCpp (Literal _ (ObjectLiteral ps)) =
    CppObjectLiteral <$> sortBy (compare `on` fst) <$> mapM (sndM valueToCpp) ps

  -- TODO: Change how this is done when proper Meta info added
  valueToCpp (Accessor _ prop val)
    | "__superclass_" `isPrefixOf` prop = valueToCpp val

  valueToCpp (Accessor _ prop val)
    | Just (Map pairs) <- typFromExpr val = do
      case lookup prop pairs of
        Just _ -> do
          val' <- fnVarToCpp val
          return $ CppMapAccessor (CppStringLiteral prop) val'
        _ -> error $ "Bad record name: " ++ prop

  valueToCpp (Accessor _ prop val) =
    CppIndexer <$> pure (CppVar prop) <*> valueToCpp val

  valueToCpp (ObjectUpdate _ obj ps) = do
    obj' <- valueToCpp obj
    ps' <- mapM (sndM valueToCpp) ps
    extendObj obj' ps'

  valueToCpp e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
    error $ "** IsTypeClassConstructor **\n" ++ show e

  valueToCpp (Abs (_, _, ty, _) arg body) = do
    let typ = ty >>= mktype mn
    cpp <- valueToCpp body
    return $ CppLambda [CppCaptureAll] [(identToCpp arg, argtype typ)] (rettype typ) (asReturnBlock cpp)

  valueToCpp (App _ e (Var _ (Qualified (Just (ModuleName [ProperName "Prim"])) (Ident "undefined")))) =
    valueToCpp e

  valueToCpp e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM fnVarToCpp args
    case f of
      Var (_, _, _, Just IsNewtype) _ ->
        return (head args')
      Var (_, _, Just ty, Just (IsConstructor _ fields)) name ->
        let fieldCount = length fields
            argsNotApp = fieldCount - length args
            qname = qualifiedToStr' id name
            tmplts = maybe [] getDataTypeArgs (mktype mn ty >>= getDataType qname)
            val = CppDataConstructor qname tmplts in
        if argsNotApp > 0
          then let argTypes = maybe [] (init . fnTypesN fieldCount) (mktype mn ty) in
               return (CppPartialApp val args' argTypes argsNotApp)
          else return (CppApp val args')
      Var (_, _, _, Just IsTypeClassConstructor) _ ->
        return CppNoOp
      Var (_, _, Just _, _) (Qualified (Just _) _) ->
        fnApp e
      Var (_, _, Nothing, _) ident ->
        case findInstance ident of
          Nothing -> fnApp e
          _       -> valueToCpp f
      Abs (_, _, Just _, _) _ body ->
        fnApp body
      _ -> -- TODO: verify this
        flip (foldl (\fn a -> CppApp fn [a])) args' <$> valueToCpp f

  valueToCpp (Case (maybeSpan, _, _, _) values binders) = do
    vals <- mapM valueToCpp values
    bindersToCpp maybeSpan binders vals

  valueToCpp (Let _ ds val) = do
    ds' <- concat <$> mapM bindToCpp ds
    ret <- valueToCpp val
    return $ CppApp (CppLambda [CppCaptureAll] [] Nothing (CppBlock (ds' ++ [CppReturn ret]))) []

  valueToCpp (Constructor {}) =
    return CppNoOp

  -- |
  -- Shallow copy an object.
  --
  -------------------------------------------------------------------------------------------------
  extendObj :: Cpp -> [(String, Cpp)] -> m Cpp
  -------------------------------------------------------------------------------------------------
  extendObj = error "Extend obj TBD"
  -- extendObj obj sts = do
  --   newObj <- freshName
  --   key <- freshName
  --   let
  --     cppKey = CppVar key
  --     cppNewObj = CppVar newObj
  --     block = CppBlock (objAssign:copy:extend ++ [CppReturn cppNewObj])
  --     objAssign = CppVariableIntroduction newObj (Just $ CppObjectLiteral [])
  --     copy = CppForIn key obj $ CppBlock [CppIfElse cond assign Nothing]
  --     cond = CppApp (CppAccessor "hasOwnProperty" obj) [cppKey]
  --     assign = CppBlock [CppAssignment (CppIndexer cppKey cppNewObj) (CppIndexer cppKey obj)]
  --     stToAssign (s, cpp) = CppAssignment (CppAccessor s cppNewObj) cpp
  --     extend = map stToAssign sts
  --   return $ CppApp (CppFunction Nothing [] block) []

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
  bindersToCpp :: Maybe SourceSpan -> [CaseAlternative Ann] -> [Cpp] -> m Cpp
  -------------------------------------------------------------------------------------------------
  bindersToCpp maybeSpan binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith mkVarDecl valNames (map Just vals)
    cpps <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToCpp result
      go valNames ret bs
    return $ CppApp (CppLambda [CppCaptureAll]
                               []
                               Nothing
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
    return (CppVariableIntroduction (identToCpp ident, ty >>= mktype mn) [] [] (Just (CppVar varName)) : done)

  binderToCpp varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToCpp varName done b

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
    return (CppVariableIntroduction (identToCpp ident, ty >>= mktype mn) [] [] (Just (CppVar varName)) : cpp)

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
    return [CppIfElse (CppVar varName) (CppBlock done) Nothing]

  literalToBinderCpp varName done (BooleanLiteral False) =
    return [CppIfElse (CppUnary CppNot (CppVar varName)) (CppBlock done) Nothing]

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
                                      (Just (CppMapAccessor (CppStringLiteral prop) (CppVar varName)))
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
  mkTypeClass :: Monad m => Ident -> Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  mkTypeClass (Ident ctor) (_, comms, _, Just IsTypeClassConstructor)
    | Just (params, constraints, fns) <- findClass (Qualified (Just mn) (ProperName ctor)) = do
    let tmplts = runType . mkTemplate <$> params
        fnTemplPs = nub $ (concatMap (templparams' . mktype mn . snd) fns) ++
                          (concatMap constraintParams constraints)
        classTemplParams = zip tmplts $ fromMaybe 0 . flip lookup fnTemplPs <$> tmplts
    cpps' <- mapM (toCpp tmplts) fns
    let struct' = CppStruct (ctor, classTemplParams ++ [("...", 0)])
                            []
                            (constraintInfo [] <$> constraints)
                            cpps'
                            []
    return (CppComment comms struct')
    where
    tmpParams :: [(String, T.Type)] -> [TemplateInfo]
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
  mkTypeClass _ _ = return CppNoOp

  -------------------------------------------------------------------------------------------------
  mkInstance :: Ident -> Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  mkInstance ident expr
    | Just (classname@(Qualified (Just classmn) (ProperName unqualClass)), typs) <- findInstance (Qualified (Just mn) ident),
      Just (params, constraints, fns) <- findClass classname = do
    let (_, fs) = case expr of
                    App{} -> unApp expr []
                    Abs _ _ e'@(App{}) -> unApp e' []
                    Var (_, _, _, Just IsTypeClassConstructor) _ -> (expr, [])
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
                           (constraintInfo tymap <$> constraints)
                           cpps
                           []
    return $ if classmn == mn
               then struct
               else CppNamespace ("::" ++ runModuleName classmn) [CppUseNamespace (runModuleName mn), struct]
    where
    toCpp :: [TemplateInfo] -> ((String, T.Type), Expr Ann) -> m Cpp
    toCpp tmplts ((name, ty'), Abs ann'@(_, _, Just _, _) a b) = do
      fn <- mkFunction tmplts (Ident name) ann' a b [CppStatic]
      return $ case mktype mn ty' of Just Function {} -> fn
                                     Just _ -> toLambda [] [] fn
                                     _ -> fn

    toCpp tmplts ((name, _), e) -- convert these vars to inline functions
      | Just ty <- tyFromExpr e,
        Just Function {} <- mktype mn ty
        = asFunction tmplts (Ident name) (Nothing, [], Just ty, Nothing) e [CppInline, CppStatic]

    toCpp tmplts ((name, _), e)
      | Just ty <- tyFromExpr e,
        typ <- mktype mn ty,
        tmplts'@(_:_) <- templparams' typ = do
        e' <- valueToCpp e
        let cpp' | Just typ' <- typ,
                   vs@(_:_) <- templateVars typ',
                   not (valueHasTemplates e') = Just (asTemplate vs e')
                 | otherwise = Just e'
        return $ CppVariableIntroduction (name, typ) (tmplts' \\ tmplts) [CppStatic] cpp'
    toCpp _ ((name, _), e)
      | Just ty <- tyFromExpr e = do
        e' <- valueToCpp e
        let typ = mktype mn ty
        return $ CppVariableIntroduction (name, typ) [] [CppStatic] (Just e')
    toCpp _ ((name, _), e) = return $ error $ (name ++ " :: " ++ show e ++ "\n")

    isDataTypeCtor :: [Type] -> Bool
    isDataTypeCtor ts = any go ts
      where
      go :: Type -> Bool
      go (Native nname nts@(_:_))
        | Just (_, tk) <- M.lookup (Qualified (Just mn) (ProperName nname)) (E.types env),
          E.DataType ps _ <- tk = length ps /= length nts
      go _ = False

    templatesFromKinds :: [Type] -> [TemplateInfo]
    templatesFromKinds ts = concatMap go ts
      where
      go :: Type -> [TemplateInfo]
      go (Native nname nts@(_:_))
        | Just (_, tk) <- M.lookup (Qualified (Just mn) (ProperName nname)) (E.types env),
          E.DataType ps _ <- tk = catMaybes $ swapNames <$> zip (templateFromKind <$> ps) nts
      go _ = []
      swapNames :: (TemplateInfo, Type) -> Maybe TemplateInfo
      swapNames ((_, n), (Template t _)) = Just (runType (Template t []), n)
      swapNames _ = Nothing

    isNormalFn :: (Expr Ann) -> Bool
    isNormalFn (Abs _ (Ident "__unused") e) | Nothing <- tyFromExpr e = False
    isNormalFn _ = True

  mkInstance ident _ = error ("Instance \"" ++ show ident ++ "\" not found")

  -------------------------------------------------------------------------------------------------
  fnVarToCpp :: Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  fnVarToCpp (Var (_, _, Just ty, _) qid@(Qualified mn' ident))
    | Just typ <- mktype mn ty,
      Just vty <- findValue (fromMaybe mn mn') ident,
      Just declType <- mktype mn vty,
      tmplts@(_:_) <- snd <$> templateMappings (declType, typ) =
      return $ asTemplate tmplts (varToCpp qid)
  fnVarToCpp v = valueToCpp v

  -------------------------------------------------------------------------------------------------
  fnApp :: Expr Ann -> m Cpp
  -------------------------------------------------------------------------------------------------
  fnApp e = do
      let (f, args) = unApp e []
      f' <- valueToCpp f
      args' <- mapM fnVarToCpp args
      let fn = P.prettyPrintCpp [f']
          instArgs = filter (instanceArg fn) args'
          normArgs = filter normalArg args'
      let declType = (listToMaybe instArgs >>= instanceFnType fn) <|> typFromExpr f <|> findFnDeclType f
          exprType = fnTypFromApp e
          allTemplates = fromMaybe [] $ templateMappings <$> (liftM2 (,) declType exprType)
          templateChanges = onlyChanges allTemplates
          instArgs' = fixInstArg templateChanges <$> instArgs
          instanceTmplts = concatMap paramTmplts instArgs
          tmplts = snd <$> filter (\(t, _) -> t `notElem` instanceTmplts) allTemplates
      return $ flip (foldl (\fn' a -> CppApp fn' [a])) (instArgs' ++ normArgs) (asTemplate tmplts f')
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
                                   [(_,t2)] <- onlyChanges $ templateMappings (t, t') = t2
        go t = t
      instanceFnType fname (CppApp i@CppInstance{} _) = instanceFnType fname i
      instanceFnType _ _ = Nothing

      findFnDeclType :: Expr Ann -> Maybe Type
      findFnDeclType (Var _ (Qualified mn' ident))
        | (Just ty) <- findValue (fromMaybe mn mn') ident, typ@(Just _) <- mktype mn ty = typ
      findFnDeclType _ = Nothing

      paramTmplts :: Cpp -> [Type]
      paramTmplts (CppInstance _ _ _ ps) = concatMap templateVars (catMaybes $ snd <$> ps)
      paramTmplts _ = []

      fixInstArg :: [(Type, Type)] -> Cpp -> Cpp
      fixInstArg [] cpp = cpp
      fixInstArg mappings (CppInstance mn' cls iname ps) = CppInstance mn' cls iname (go <$> ps)
        where
        go :: (String, Maybe Type) -> (String, Maybe Type)
        go (pn, Just pty) = (pn, Just (replaceTypes pty))
        go p = p
        replaceTypes :: Type -> Type
        replaceTypes = everywhereOnTypes go'
          where
          go' :: Type -> Type
          go' t@(Template {}) | Just t' <- lookup t mappings = t'
          go' t = t
      fixInstArg mappings (CppApp i@CppInstance{} _) = fixInstArg mappings i
      fixInstArg _ cpp = cpp

  -------------------------------------------------------------------------------------------------

  unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
  unApp (App _ val arg) args = unApp val (arg : args)
  unApp other args = (other, args)

  typFromExpr :: Expr Ann -> Maybe Type
  typFromExpr expr = tyFromExpr expr >>= mktype mn

  fnTypFromApp :: Expr Ann -> Maybe Type
  fnTypFromApp (App (_, _, Just ty, _) val a)
    | Just nextTy <- fnTypFromApp val = Just nextTy
    | Just a' <- typFromExpr a,
      Just b' <- mktype mn ty = Just $ Function a' b'
    | Just t' <- mktype mn ty = Just t'
  fnTypFromApp (App (_, _, Nothing, _) val _) = fnTypFromApp val
  fnTypFromApp _ = Nothing

  qualifiedToCpp :: (a -> Ident) -> Qualified a -> Cpp
  qualifiedToCpp f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = CppVar . runIdent $ f a
  qualifiedToCpp f (Qualified (Just mn') a)
    | mn /= mn' = CppAccessor Nothing (CppVar . identToCpp $ f a) (CppScope (moduleNameToCpp mn'))
  qualifiedToCpp f (Qualified _ a) = CppVar $ identToCpp (f a)

  qualifiedToStr' :: (a -> Ident) -> Qualified a -> String
  qualifiedToStr' = qualifiedToStr mn

  constraintInfo :: [(Type, Type)] -> (Qualified ProperName, [T.Type]) -> (String, [Type])
  constraintInfo [] (name, tys) = (qualifiedToStr' (Ident . runProperName) name, catMaybes $ mktype mn <$> tys)
  constraintInfo tymap (name, tys) =
    (qualifiedToStr' (Ident . runProperName) name, replace <$> catMaybes (mktype mn <$> tys))
    where
    replace :: Type -> Type
    replace t | Just t' <- lookup t tymap = t'
    replace t = t

  -- |
  -- Find a type class instance in scope by name, retrieving its class name and construction types.
  --
  -- TODO: see if we can provide typeclass to make this faster
  --
  findInstance :: Qualified Ident -> Maybe (Qualified ProperName, [Maybe Type])
  findInstance ident@(Qualified (Just mn') _)
    | Just classMap <- M.lookup (Just mn) (E.typeClassDictionaries env),
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
    | Just (params, fns, constraints) <- M.lookup name (E.typeClasses env),
      fns' <- (\(i,t) -> (identToCpp i, t)) <$> fns
      = Just (fst <$> params, constraints, (sortBy (compare `on` normalizedName . fst) fns'))
  findClass _ = Nothing

  -- |
  -- Find a value (incl functions) in scope by name, retrieving its type
  --
  findValue :: ModuleName -> Ident -> Maybe T.Type
  findValue mname ident
    | Just (ty, _, _) <- M.lookup (mname, ident) (E.names env) = Just ty
  findValue _ _ = Nothing
