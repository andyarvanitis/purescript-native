-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code in the simplified Javascript intermediate representation from Purescript code
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CodeGen.JS (
    module AST,
    module Common,
    moduleToJs
) where

import Data.List ((\\), delete)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, nubBy, sortBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Traversable as T (traverse)

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad (foldM, replicateM, forM)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.Optimizer
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Types as T

import Language.PureScript.CodeGen.Cpp
import Debug.Trace

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => Module Ann -> m [JS]
moduleToJs (Module coms name imps exps foreigns decls) = do
--  additional <- lift $ asks optionsAdditional
  let imps' = delete (ModuleName [ProperName C.prim]) . (\\ [name]) $ imps
  jsImports <- T.traverse importToJs $ imps'
  preambleImport <- importToJs $ preambleHeader
  ownImport <- importToJs $ name
  let foreigns' = mapMaybe (\(_, js, _) -> js) foreigns
  jsDecls <- mapM (bindToJs name) decls
  optimized <- T.traverse (T.traverse optimize) jsDecls
  let isModuleEmpty = null exps
  let (moduleDecls, moduleBody, extTempls, templs) = sections $ concat optimized
  let moduleHeader = (JSRaw . (++ ";") . ("using namespace " ++) . moduleNameToJs <$> imps')
                  ++ [JSRaw " "]
                  ++ dataTypes decls
                  ++ moduleDecls
                  ++ foreigns'
                  ++ (if not $ null extTempls then
                        [JSRaw ("#ifndef " ++ moduleNameToJs name ++ "_CC")]
                     ++ extTempls
                     ++ [JSRaw ("#endif // " ++ moduleNameToJs name ++ "_CC\n")]
                      else []
                     )
                  ++ templs
  return $
         [ JSRaw $ "#ifndef " ++ moduleNameToJs name ++ "_H"
         , JSRaw $ "#define " ++ moduleNameToJs name ++ "_H\n"
         ]
      ++ (if isPrelude name then [preambleImport] else [])
      ++ jsImports
      ++ [ JSRaw "//"
         , JSNamespace (moduleNameToJs name) moduleHeader
         , JSRaw $ "#endif // " ++ moduleNameToJs name ++ "_H"
         , JSEndOfHeader
         ]
      ++ [ JSRaw $ "#define " ++ moduleNameToJs name ++ "_CC\n"
         , ownImport
         , JSRaw "//"
         , JSNamespace (moduleNameToJs name) moduleBody
         ]
      ++ (if isMain name then nativeMain else [])
-- |
-- Generates Javascript code for a module import.
--
importToJs :: (Monad m, MonadReader (Options mode) m) => ModuleName -> m JS
importToJs mn = do
  additional <- asks optionsAdditional
  return $ JSRaw $ "#include " ++ '"' : (dotsTo '/' $ runModuleName mn)
                               ++ '/' : (last . words . dotsTo ' ' $ runModuleName mn) ++ ".hh\""
  where
    dotsTo :: Char -> String -> String
    dotsTo chr = map (\c -> if c == '.' then chr else c)
-- |
-- Generate code in the simplified Javascript intermediate representation for a declaration
--
bindToJs :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
         => ModuleName -> Bind Ann -> m [JS]
bindToJs mp (NonRec ident val) = return <$> nonRecToJS mp ident val
bindToJs mp (Rec vals) = forM vals (uncurry (nonRecToJS mp))

-- |
-- Generate code in the simplified Javascript intermediate representation for a single non-recursive
-- declaration.
--
-- The main purpose of this function is to handle code generation for comments.
--
nonRecToJS :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => ModuleName -> Ident -> Expr Ann -> m JS
nonRecToJS m i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
  withoutComment <- asks optionsNoComments
  if withoutComment
     then nonRecToJS m i (modifyAnn removeComments e)
     else JSComment com <$> nonRecToJS m i (modifyAnn removeComments e)

nonRecToJS mp ident val@(App{}) | (f, n) <- dropApp val,
                                  (Var (_, _, _, Just (IsConstructor _ fields)) _) <- f,
                                  n == length fields = do
  js <- valueToJs mp val
  return $ JSVariableIntroduction (identToJs ident) (Just js)

nonRecToJS mp ident val@(App{}) | (f, n) <- dropApp val,
                                  (Var (_, _, _, Just IsNewtype) _) <- f,
                                  n == 1 = do
  js <- valueToJs mp val
  return $ JSVariableIntroduction (identToJs ident) (Just js)

nonRecToJS mp ident val = do
  js <- valueToJs mp val
  case js of
    JSSequence [] jss -> return $ JSSequence (identToJs ident) jss
    JSVar{} -> do
      js' <- valueToJs mp (valToAbs val)
      return $ JSVariableIntroduction (identToJs ident) (Just js')
    JSApp{} -> do
      js' <- valueToJs mp (valToAbs val)
      return $ JSVariableIntroduction (identToJs ident) (Just js')
    _ -> return $ JSVariableIntroduction (identToJs ident) (Just js)

-- |
-- Generate code in the simplified Javascript intermediate representation for a variable based on a
-- PureScript identifier.
--
var :: Ident -> JS
var = JSVar . identToJs

-- |
-- Generate code in the simplified Javascript intermediate representation for an accessor based on
-- a PureScript identifier. If the name is not valid in Javascript (symbol based, reserved name) an
-- indexer is returned.
--
accessor :: Ident -> JS -> JS
accessor (Ident prop) = accessorString prop
accessor (Op op) = JSIndexer (JSStringLiteral op)

accessorString :: String -> JS -> JS
accessorString prop | identNeedsEscaping prop = JSIndexer (JSStringLiteral prop)
                    | otherwise = JSAccessor prop

-- |
-- Generate code in the simplified Javascript intermediate representation for a value or expression.
--
valueToJs :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
          => ModuleName -> Expr Ann -> m JS
valueToJs m (Literal tt l)
  | (_, _, Just ty, _) <- tt,
    (T.TypeApp
      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Array")))
        a) <- ty = do
  literals <- literalToValueJS m l
  return $ JSApp (JSVar $ typestr m ty) [literals]
valueToJs m (Literal _ l) =
  literalToValueJS m l
valueToJs m (Var (_, _, ty, Just (IsConstructor _ [])) name) =
  return $ JSApp (JSVar . mkDataFn $ qualifiedToStr m mkUnique' name ++ (getSpecialization $ fnRetStr m ty)) []
valueToJs m (Var (_, _, ty, Just (IsConstructor _ _)) name) =
  return $ JSVar . mkDataFn $ qualifiedToStr m mkUnique' name ++ (getSpecialization $ fnRetStr m ty)
valueToJs m (Accessor _ prop val) =
  (accessorString prop . JSFromPtr) <$> valueToJs m val
valueToJs m (ObjectUpdate _ o ps) = do
  obj <- valueToJs m o
  sts <- mapM (sndM $ valueToJs m) ps
  extendObj obj sts
valueToJs m e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
  let args = unAbs e
  in return $ JSSequence [] (toFn <$> args)
  where
  unAbs :: Expr Ann -> [(Ident, Maybe T.Type)]
  unAbs (Abs (_, _, ty, _) arg val) = (arg, ty) : unAbs val
  unAbs _ = []

  toFn :: (Ident, Maybe T.Type) -> JS
  toFn (ident, ty@(Just _)) = JSVariableIntroduction (identToJs ident) (mkfunc ident ty)
  toFn _ = JSNoOp

  mkfunc ident ty
    | arg@(_:_) <- fnArgStr m ty = Just $ JSFunction (annotatedName ident ty $ fnRetStr m ty) [arg] JSNoOp
    | Just ty' <- ty = Just $ JSFunction (annotatedName ident ty $ typestr m ty') [] JSNoOp
    | otherwise = Just JSNoOp
  annotatedName ident ty rty = Just $ templTypes' m ty ++ rty ++ ' ' : (identToJs ident)

valueToJs m (Abs (_, _, (Just (T.ConstrainedType ts _)), _) _ val)
    | (Abs (_, _, t, _) _ val') <- val, Nothing <- t = valueToJs m (dropAbs (length ts - 2) val') -- TODO: confirm '-2'
    | otherwise = valueToJs m val
    where
      dropAbs :: Int -> Expr Ann -> Expr Ann
      dropAbs n (Abs _ _ ann) | n > 0 = dropAbs (n-1) ann
      dropAbs _ a = a
valueToJs m (Abs (_, _, Just ty, _) arg val) | isConstrained ty = return JSNoOp
  where
    isConstrained :: T.Type -> Bool
    isConstrained (T.ForAll _ (T.ConstrainedType _ _) _) = True
    isConstrained (T.ForAll _ t _) = isConstrained t
    isConstrained _ = False
valueToJs m (Abs (_, _, ty, _) arg val) = do
  ret <- valueToJs m val
  return $ JSFunction annotatedName [argtype ++ argname] (JSBlock [JSReturn ret])
  where
    argtype | Nothing <- ty, [aty] <- atys = typestr m aty
            | otherwise = fnArgStr m ty
    argname | name@(_:_) <- identToJs arg = ' ' : name
            | otherwise = []

    atys = nub . f $ NonRec arg val
    (f, _, _, _) = everythingOnValues (++) (const []) values (const []) (const [])
    values :: Expr Ann -> [T.Type]
    values (Var (_, _, Just t, _) (Qualified _ ident)) | ident == arg = [t]
    values _ = []

    annotatedName
      | Just ty' <- ty, Ident [] <- arg = Just $ templTypes' m ty ++ typestr m ty' ++ " _"
      | typ@(_:_) <- fnRetStr m ty = Just $ templTypes' m ty ++ typ ++ " _"
      | otherwise = Nothing

valueToJs m e@App{} = do
  let (f, args) = unApp e []
  args' <- mapM (valueToJs m) (filter (not . typeinst) args)
  case f of
    Var (_, _, _, Just IsNewtype) name ->
      let dataName = qualifiedToStr m mkUnique' name ++ getAppSpecType m e 0 in
      return $ JSApp (JSVar . mkData $ dataName) (dataFields dataName $ take 1 args')
    Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
      let dataName = qualifiedToStr m mkUnique' name ++ getAppSpecType m e 0 in
      return $ JSApp (JSVar . mkData $ dataName) (dataFields dataName args')
    Var (_, _, _, Just (IsConstructor _ fields)) name | not (null args) ->
      return $ foldl (\fn a -> JSApp fn [a]) (JSVar . mkDataFn $ qualifiedToStr m mkUnique' name
                                                              ++ getAppSpecType m e (length fields - length args + 1)) args'
    Var (_, _, ty, Just IsTypeClassConstructor) name'@(Qualified mn (Ident name)) -> do
      convArgs <- mapM (valueToJs m) (instFn name' args)
      let convArgs' = map toVarDecl (depSort $ zip (names ty) convArgs)
      return $ JSSequence ("instance " ++ (rmType name) ++ ' ' : (intercalate " " $ typeclassTypeNames m e name')) convArgs'
    _ -> if null args then flip JSApp [] <$> fn'
         else flip (foldl (\fn a -> JSApp fn [a])) args' <$> fn'
      where
        fn' | isQualified f || typeinst (head args) = specialized' f (head args)
            | otherwise = valueToJs m f
  where
  unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
  unApp (App _ val arg) args = unApp val (arg : args)
  unApp other args = (other, args)

  names ty = map fst (fst . T.rowToList $ fromMaybe T.REmpty ty)
  toVarDecl :: (String, JS) -> JS
  toVarDecl (nm, js) | JSFunction _ _ _ <- js, C.__superclass_ `isPrefixOf` nm = JSNoOp
  toVarDecl (nm, js) =
    JSVariableIntroduction (identToJs $ Ident nm)
                           (Just $ case js of
                                     JSFunction orig ags sts -> JSFunction (toTempl orig) ags sts
                                     _ -> js)
    where
      toTempl fn | fn' <- fromMaybe "" fn = if '|' `elem` fn' then fn else Just ('|' : fn')

  typeinst :: Expr Ann -> Bool
  typeinst (Var (_, _, Nothing, Nothing) _) = True -- TODO: make sure this doesn't remove the wrong (untyped) args
  typeinst (Accessor (_, _, Nothing, Nothing) _ v) = typeinst v
  typeinst (App (_, _, Nothing, Nothing) _ v) = typeinst v
  typeinst _ = False

  instFn :: Qualified Ident -> [Expr Ann] -> [Expr Ann]
  instFn name = map $ convExpr (convType $ typeclassTypes e name) . valToAbs

  specialized name ty = name ++ templateSpec (declFnTy m e) (exprFnTy m e)

  specialized' f@(Var (_, _, ty, _) (Qualified q ident)) a =
    pure . JSVar $ specialized (qualifiedToStr m id (Qualified qual ident)) ty
    where
      qual | (Var _ (Qualified _ _)) <- a, typeinst a = Nothing
           | otherwise = q
  specialized' f a = pure . JSVar $ traceShow f []

  isQualified :: Expr Ann -> Bool
  isQualified (Var _ (Qualified (Just _) _)) = True
  isQualified _ = False

valueToJs m (Var (_, _, ty, Just IsNewtype) ident) =
  return $ JSVar . mkDataFn $ qualifiedToStr m (mkUnique' . mkUnique') ident ++ (getSpecialization $ fnRetStr m ty)

valueToJs m (Var (_, _, Just ty, _) ident) =
  return $ varJs m ident
  where
    varJs :: ModuleName -> Qualified Ident -> JS
    varJs _ (Qualified Nothing ident) = JSVar $ identToJs ident ++ addType (typestr m ty)
    varJs m qual = JSVar $ (qualifiedToStr m id qual) ++ addType (typestr m ty)
valueToJs m (Var _ ident) =
  return $ varToJs m ident
valueToJs m (Case _ values binders) = do
  vals <- mapM (valueToJs m) values
  bindersToJs m binders vals
valueToJs m (Let _ ds val) = do
  decls <- concat <$> mapM (bindToJs m) ds
  ret <- valueToJs m val
  return $ JSApp (JSFunction Nothing [] (JSBlock ((asLambda <$> zip ds decls) ++ [JSReturn ret]))) []
  where
    -- TODO: refactor this (shouldn't be adding qualifiers here or relying on them later)
    asLambda (d, JSVariableIntroduction v (Just (JSFunction name args ret))) =
      JSVariableIntroduction ("static " ++ absType d ++ " const " ++ v) (Just $ JSFunction name args ret)
    asLambda (_, js) = js
    absType d
      | (NonRec _ (Abs (_, _, Just ty, _) _ _)) <- d = typestr m ty
      | otherwise = []

valueToJs m (Constructor (_, _, Just ty, Just IsNewtype) (ProperName typename) (ProperName ctor) _) =
  return $ JSData (mkUnique ctor) typename [typestr m ty] JSNoOp
valueToJs m (Constructor (_, _, Just ty, _) (ProperName typename) (ProperName ctor) []) =
  return $ JSData (mkUnique ctor) typename [] $
             JSVariableIntroduction dataCtorName $ Just $
               JSFunction (Just $ typestr m ty ++ ' ' : dataCtorName)
                 [fnRetStr m (Just ty)] (JSBlock [JSReturn $ JSApp (JSVar . mkData $ mkUnique ctor) []])
valueToJs m (Constructor (_, _, Just ty, _) (ProperName typename) (ProperName ctor) fields) =
  return $ JSData (mkUnique ctor) typename fields' $
             JSVariableIntroduction dataCtorName $ Just $
               JSFunction (Just $ fnRetStr m (Just ty) ++ ' ' : dataCtorName)
                 [head types ++ ' ' : head names] (JSBlock [JSReturn (ctorBody $ tail fields')])
  where
    types = typestr m <$> (fieldTys ty)
    names = identToJs <$> fields
    fields' = zipWith (\t n -> t ++ ' ' : n) types names

    fieldTys (T.TypeApp
               (T.TypeApp
                 (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                   a) b) = typs a [] ++ fieldTys b
      where
        typs ty@(T.TypeApp
                  (T.TypeApp
                    (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                      _) _) tys = tys ++ [ty]
        typs ty tys = tys ++ [ty]
    fieldTys (T.ForAll _ ty _) = fieldTys ty
    fieldTys _ = []

    ctorBody :: [String] -> JS
    ctorBody (arg:args) = JSFunction Nothing [arg] $ JSBlock [JSReturn $ ctorBody args]
    ctorBody [] = JSApp (JSVar . mkData $ mkUnique ctor) (JSVar <$> names)

iife :: String -> [JS] -> JS
iife v exprs = JSApp (JSFunction Nothing [] (JSBlock $ exprs ++ [JSReturn $ JSVar v])) []

literalToValueJS :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
                 => ModuleName -> Literal (Expr Ann) -> m JS
literalToValueJS _ (NumericLiteral n) = return $ JSNumericLiteral n
literalToValueJS _ (StringLiteral s) = return $ JSStringLiteral s
literalToValueJS _ (BooleanLiteral b) = return $ JSBooleanLiteral b
literalToValueJS m (ArrayLiteral xs) = JSArrayLiteral <$> mapM (valueToJs m) xs
literalToValueJS m (ObjectLiteral ps) = do
  jss <- mapM (valueToJs m . snd) ps
  let names = toField <$> ps
  return $ JSObjectLiteral $ zip names jss
  where
  toField (name, Literal (_, _, Just ty, _) _) = typestr m ty ++ ' ' : name
  toField (name, _) = name

-- |
-- Shallow copy an object.
--
extendObj :: (Applicative m, Monad m, MonadSupply m) => JS -> [(String, JS)] -> m JS
extendObj obj sts = do
  newObj <- freshName
  key <- freshName
  let
    jsKey = JSVar key
    jsNewObj = JSVar newObj
    block = JSBlock (objAssign:copy:extend ++ [JSReturn jsNewObj])
    objAssign = JSVariableIntroduction newObj (Just $ JSObjectLiteral [])
    copy = JSForIn key obj $ JSBlock [JSIfElse cond assign Nothing]
    cond = JSApp (JSAccessor "hasOwnProperty" obj) [jsKey]
    assign = JSBlock [JSAssignment (JSIndexer jsKey jsNewObj) (JSIndexer jsKey obj)]
    stToAssign (s, js) = JSAssignment (JSAccessor s jsNewObj) js
    extend = map stToAssign sts
  return . error . show $ JSApp (JSFunction Nothing [] block) []

-- |
-- Generate code in the simplified Javascript intermediate representation for a reference to a
-- variable.
--
varToJs :: ModuleName -> Qualified Ident -> JS
varToJs _ (Qualified Nothing ident) = var ident
varToJs m qual = qualifiedToJS m id qual

-- |
-- Generate code in the simplified Javascript intermediate representation for a reference to a
-- variable that may have a qualified name.
--
qualifiedToJS :: ModuleName -> (a -> Ident) -> Qualified a -> JS
qualifiedToJS _ f (Qualified (Just (ModuleName [ProperName mn])) a) | mn == C.prim = JSVar . runIdent $ f a
qualifiedToJS m f (Qualified (Just m') a) | m /= m' = (JSVar (moduleNameToJs m' ++ "::" ++ identToJs (f a)))
qualifiedToJS _ f (Qualified _ a) = JSVar $ identToJs (f a)

-- |
-- Generate code in the simplified Javascript intermediate representation for pattern match binders
-- and guards.
--
bindersToJs :: forall m mode. (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
            => ModuleName -> [CaseAlternative Ann] -> [JS] -> m JS
bindersToJs m binders vals = do
  untypedValNames <- replicateM (length vals) freshName
  let valNames = copyTyInfo <$> zip untypedValNames vals
  let assignments = zipWith JSVariableIntroduction valNames (map Just vals)
  fn' <- forM binders $ \(CaseAlternative bs result) -> do
    ret <- guardsToJs result
    ret' <- go valNames ret bs
    return (ret', retType result)
  let jss = fst <$> fn'
  let name = listToMaybe . filter (not . null) $ map snd fn'
  return $ JSApp (JSFunction Nothing [] (JSBlock (assignments ++ concat jss ++ [JSThrow $ JSStringLiteral "Failed pattern match"])))
                 []
  where
    go :: [String] -> [JS] -> [Binder Ann] -> m [JS]
    go _ done [] = return done
    go (v:vs) done' (b:bs) = do
      done'' <- go vs done' bs
      binderToJs m v done'' b
    go _ _ _ = error "Invalid arguments to bindersToJs"

    guardsToJs :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [JS]
    guardsToJs (Left gs) = forM gs $ \(cond, val) -> do
      cond' <- valueToJs m cond
      done  <- valueToJs m val
      return $ JSIfElse cond' (JSBlock [JSReturn done]) Nothing
    guardsToJs (Right v) = return . JSReturn <$> valueToJs m v

    copyTyInfo :: (String, JS) -> String
    copyTyInfo (s, JSVar v) = s ++ getType v
    copyTyInfo (s, _) = s

    retType :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> String
    retType (Right (App (_, _, Just ty, _) _ _)) = typestr m ty ++ " f"
    retType (Left vs@(_:_)) = retType (Right . snd $ last vs)
    retType _ = []

-- |
-- Generate code in the simplified Javascript intermediate representation for a pattern match
-- binder.
--
binderToJs :: forall m mode. (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => ModuleName -> String -> [JS] -> Binder Ann -> m [JS]
binderToJs _ _ done (NullBinder{}) = return done
binderToJs m varName done (LiteralBinder _ l) =
  literalToBinderJS m varName done l
binderToJs _ varName done (VarBinder _ ident) =
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : done)
binderToJs m varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
  binderToJs m varName done b
binderToJs m varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
  js <- go (zip fields bs) done
  return $ case ctorType of
    ProductType -> js
    SumType ->
      [JSIfElse (JSInstanceOf (JSVar varName) (JSVar ctorName))
                (JSBlock js)
                Nothing]
  where
  go :: [(Ident, Binder Ann)] -> [JS] -> m [JS]
  go [] done' = return done'
  go ((field, binder) : remain) done' = do
    argVar <- freshName
    done'' <- go remain done'
    js <- binderToJs m argVar done'' binder
    return (JSVariableIntroduction argVar (Just (JSAccessor (identToJs field) (JSCast (JSVar ctorName) (JSVar varName)))) : js)
  ctorName = qualifiedToStr m (Ident . mkUnique . runProperName) ctor ++ getSpecialization varName
binderToJs m varName done binder@(ConstructorBinder _ _ ctor _) | isCons ctor = do
  let (headBinders, tailBinder) = uncons [] binder
      numberOfHeadBinders = fromIntegral $ length headBinders
  js1 <- foldM (\done' (headBinder, index) -> do
    headVar <- freshName
    jss <- binderToJs m headVar done' headBinder
    return (JSVariableIntroduction headVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : jss)) done (zip headBinders [0..])
  tailVar <- freshName
  js2 <- binderToJs m tailVar js1 tailBinder
  return $ JSVariableIntroduction tailVar
             (Just $ JSApp (JSAccessor "drop" (JSVar varName))
               [JSNumericLiteral (Left numberOfHeadBinders)]) : js2
  where
  uncons :: [Binder Ann] -> Binder Ann -> ([Binder Ann], Binder Ann)
  uncons acc (ConstructorBinder _ _ ctor' [h, t]) | isCons ctor' = uncons (h : acc) t
  uncons acc tailBinder = (reverse acc, tailBinder)
binderToJs _ _ _ b@(ConstructorBinder{}) =
  error $ "Invalid ConstructorBinder in binderToJs: " ++ show b
binderToJs m varName done (NamedBinder _ ident binder) = do
  js <- binderToJs m varName done binder
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : js)

literalToBinderJS :: forall m mode. (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
                  => ModuleName -> String -> [JS] -> Literal (Binder Ann) -> m [JS]
literalToBinderJS _ varName done (NumericLiteral num) =
  return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSNumericLiteral num)) (JSBlock done) Nothing]
literalToBinderJS _ varName done (StringLiteral str) =
  return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSStringLiteral str)) (JSBlock done) Nothing]
literalToBinderJS _ varName done (BooleanLiteral True) =
  return [JSIfElse (JSVar varName) (JSBlock done) Nothing]
literalToBinderJS _ varName done (BooleanLiteral False) =
  return [JSIfElse (JSUnary Not (JSVar varName)) (JSBlock done) Nothing]
literalToBinderJS m varName done (ObjectLiteral bs) = go done bs
  where
  go :: [JS] -> [(String, Binder Ann)] -> m [JS]
  go done' [] = return done'
  go done' ((prop, binder):bs') = do
    propVar <- freshName
    done'' <- go done' bs'
    js <- binderToJs m propVar done'' binder
    return (JSVariableIntroduction propVar (Just (accessorString prop (JSFromPtr $ JSVar varName))) : js)
literalToBinderJS m varName done (ArrayLiteral bs) = do
  js <- go done 0 bs
  return [JSIfElse (JSBinary EqualTo (JSAccessor "size()" (JSVar varName)) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
  where
  go :: [JS] -> Integer -> [Binder Ann] -> m [JS]
  go done' _ [] = return done'
  go done' index (binder:bs') = do
    elVar <- freshName
    done'' <- go done' (index + 1) bs'
    js <- binderToJs m elVar done'' binder
    return (JSVariableIntroduction elVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : js)

isCons :: Qualified ProperName -> Bool
isCons (Qualified (Just mn) ctor) = mn == ModuleName [ProperName C.prim] && ctor == ProperName "Array"
isCons name = error $ "Unexpected argument in isCons: " ++ show name
