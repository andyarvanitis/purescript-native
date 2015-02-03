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

{-# LANGUAGE GADTs, ViewPatterns, PatternGuards #-}

module Language.PureScript.CodeGen.JS (
    module AST,
    module Common,
    bindToJs,
    moduleToJs
) where

import Data.List ((\\), delete)
import Data.List (intercalate, isPrefixOf, nubBy, sortBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Maybe (fromMaybe, listToMaybe)

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad (foldM, replicateM, forM)

import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.Optimizer
import Language.PureScript.Options
import Language.PureScript.Supply
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Types as T

import Language.PureScript.CodeGen.Cpp
import Debug.Trace

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: (Functor m, Applicative m, Monad m) => Options mode -> Module Ann -> SupplyT m [JS]
moduleToJs opts (Module name imps exps foreigns decls) = do
  let jsImports = map (importToJs opts) . delete (ModuleName [ProperName C.prim]) . (\\ [name]) $ imps
  let foreigns' = mapMaybe (\(_, js, _) -> js) foreigns
  jsDecls <- mapM (bindToJs name) decls
  let optimized = concatMap (map $ optimize opts) jsDecls
  let isModuleEmpty = null exps
  let moduleHeader = dataTypes decls ++ (declarations <$> optimized) ++ (templates <$> optimized)
  let moduleBody = implementations <$> optimized
  let exps' = JSObjectLiteral $ (runIdent &&& JSVar . identToJs) <$> exps
  return $ case optionsAdditional opts of
    MakeOptions -> moduleBody -- ++ [JSAssignment (JSAccessor "exports" (JSVar "module")) exps']
    CompileOptions ns _ _ | not isModuleEmpty ->
      headerPreamble
      ++ (if (not.null) foreigns' then
            [JSNamespace (moduleNameToJs name ++ " /* foreign imports */") (foreigns')
            , JSRaw "// end of foreign imports"]
          else [])
      ++ [JSNamespace (moduleNameToJs name ++ " /* module header */") (moduleHeader)
        , JSRaw "// end of header"]
      ++ [JSNamespace (moduleNameToJs name ++ " /* module source */") (moduleBody)]
    _ -> []

-- |
-- Generates Javascript code for a module import.
--
importToJs :: Options mode -> ModuleName -> JS
importToJs opts mn =
  JSVariableIntroduction (moduleNameToJs mn) (Just moduleBody)
  where
  moduleBody = case optionsAdditional opts of
    MakeOptions -> JSApp (JSVar "require") [JSStringLiteral (runModuleName mn)]
    CompileOptions ns _ _ -> JSAccessor (moduleNameToJs mn) (JSVar ns)

-- |
-- Generate code in the simplified Javascript intermediate representation for a declaration
--
bindToJs :: (Functor m, Applicative m, Monad m) => ModuleName -> Bind Ann -> SupplyT m [JS]
bindToJs mp (NonRec ident val) = return <$> nonRecToJS mp ident val
bindToJs mp (Rec vals) = forM vals (uncurry (nonRecToJS mp))

-- |
-- Generate code in the simplified Javascript intermediate representation for a single non-recursive
-- declaration.
--
-- The main purpose of this function is to handle code generation for comments.
--
nonRecToJS :: (Functor m, Applicative m, Monad m) => ModuleName -> Ident -> Expr Ann -> SupplyT m JS
nonRecToJS m i e@(extractAnn -> (_, com, _, _)) | not (null com) =
  JSComment com <$> nonRecToJS m i (modifyAnn removeComments e)
nonRecToJS mp ident val = do
  js <- valueToJs mp val
  return $ JSVariableIntroduction (identToJs ident) (Just $ expr ident js)
  where
    expr :: Ident -> JS -> JS
    expr var (JSFunction orig args sts) = JSFunction (fnName orig (identToJs var)) args sts
    expr var js@(JSVar _) = expr' var js
    -- expr var (JSNamespace [] jss) = JSNamespace [] (expr' var <$> jss)
    expr var (JSSequence jss) = JSSequence (expr' var <$> jss)
    expr var js = js

    expr' :: Ident -> JS -> JS
    expr' var (JSVar name)
      | ('@':'f':'n':'<':ss) <- getType name, typ <- init ss
        = JSFunction (Just (templTypes name ++ ' ' : getRet typ ++ ' ' : identToJs var))
            [getArg typ ++ " arg"] (JSBlock [JSReturn $ JSApp (JSVar name) [JSVar "arg"]])
    expr' _ (JSVariableIntroduction var (Just js)) = JSVariableIntroduction var (Just $ expr' (Ident var) js)
    expr' _ js = js

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
valueToJs :: (Functor m, Applicative m, Monad m) => ModuleName -> Expr Ann -> SupplyT m JS
valueToJs m (Literal _ l) =
  literalToValueJS m l
valueToJs m (Var (_, _, _, Just (IsConstructor _ 0)) name) =
  return $ JSAccessor "value" $ qualifiedToJS m id name
valueToJs m (Var (_, _, ty, Just (IsConstructor _ _)) name) =
  return $ JSVar . mkDataFn $ qualifiedToStr m mkUnique' name ++ (getSpecialization $ fnRetStr m ty)
valueToJs m (Accessor _ prop val) =
  accessorString prop <$> valueToJs m val
valueToJs m (ObjectUpdate _ o ps) = do
  obj <- valueToJs m o
  sts <- mapM (sndM (valueToJs m)) ps
  extendObj obj sts
valueToJs m e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
  let args = unAbs e
  in return $ JSSequence (toFn <$> args)
  where
  unAbs :: Expr Ann -> [(Ident, Maybe T.Type)]
  unAbs (Abs (_, _, ty, _) arg val) = (arg, ty) : unAbs val
  unAbs _ = []

  toFn :: (Ident, Maybe T.Type) -> JS
  toFn (ident, ty@(Just _)) = JSVariableIntroduction (identToJs ident) (mkfunc ident ty)
  toFn _ = JSNoOp

  mkfunc ident ty
    | arg@(_:_) <- fnArgStr m ty = Just $ JSFunction (annotatedName ident ty) [arg] JSNoOp
    | otherwise = Just JSNoOp
  annotatedName ident ty = Just $ templTypes' m ty ++ fnRetStr m ty ++ ' ' : (identToJs ident)

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
  return $ JSFunction (Just annotatedName) [fnArgStr m ty ++ ' ' : identToJs arg] (JSBlock [JSReturn ret])
  where
    annotatedName = templTypes' m ty ++ fnRetStr m ty
valueToJs m e@App{} = do
  let (f, args) = unApp e []
  args' <- mapM (valueToJs m) (filter (not . typeinst) args)
  case f of
    Var (_, _, _, Just IsNewtype) name ->
      return $ JSApp (JSVar . mkData $ qualifiedToStr m mkUnique' name ++ getAppSpecType m e 0) (take 1 args')
    Var (_, _, _, Just (IsConstructor _ arity)) name | arity == length args ->
      return $ JSApp (JSVar . mkData $ qualifiedToStr m mkUnique' name ++ getAppSpecType m e 0) args'
    Var (_, _, _, Just (IsConstructor _ arity)) name | not (null args) ->
      return $ foldl (\fn a -> JSApp fn [a]) (JSVar . mkDataFn $ qualifiedToStr m mkUnique' name
                                                              ++ getAppSpecType m e (arity - length args + 1)) args'
    Var (_, _, ty, Just IsTypeClassConstructor) name'@(Qualified mn (Ident name)) -> do
      convArgs <- mapM (valueToJs m) (instFn name' args)
      return $ JSSequence $ toVarDecl <$> zip (names ty) convArgs

    _ -> flip (foldl (\fn a -> JSApp fn [a])) args' <$> if typeinst $ head args then
                                                          specialized' =<< valueToJs m f
                                                        else valueToJs m f
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
                                     JSFunction orig ags sts -> JSFunction (toTempl $ fnName orig nm) ags sts
                                     _ -> js)
    where
      toTempl fn | fn' <- fromMaybe "" fn = if '|' `elem` fn' then fn else Just ('|' : fn')

  typeinst :: Expr Ann -> Bool
  typeinst (Var (_, _, Nothing, Nothing) _) = True -- TODO: make sure this doesn't remove the wrong (untyped) args
  typeinst (Accessor (_, _, Nothing, Nothing) _ v) = typeinst v
  typeinst (App (_, _, Nothing, Nothing) _ v) = typeinst v
  typeinst _ = False

  instFn :: Qualified Ident -> [Expr Ann] -> [Expr Ann]
  instFn name = map $ convExpr (convType $ typeclassTypes e name)

  specialized (JSVar name) = JSVar $ (rmType name) ++ templateSpec (declFnTy m e) (exprFnTy m e)
  specialized' = pure . specialized

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
  return $ JSApp (JSFunction Nothing [] (JSBlock (decls ++ [JSReturn ret]))) []
valueToJs m (Constructor (_, _, Just ty, Just IsNewtype) (ProperName typename) (ProperName ctor) _) =
  return $ JSData (mkUnique ctor) typename [typestr m ty] JSNoOp
valueToJs m (Constructor (_, _, ty, _) (ProperName typename) (ProperName ctor) arity) =
    return $ JSData (mkUnique ctor) typename (fields ty) (JSVariableIntroduction [] $ Just $ mkfn fname (fields ty))
  where
    fields :: Maybe T.Type -> [String]
    fields ty = map (\(t,n) -> t ++ ' ' : ("value" ++ show n)) $ zip (types ty) ([0..] :: [Int])

    types :: Maybe T.Type -> [String]
    types Nothing = []
    types (Just (T.RCons _ ty row)) = (typestr m ty) : types (Just row)
    types (Just T.REmpty) = []

    mkfn :: Maybe String -> [String] -> JS
    mkfn name@(Just _) [] = JSFunction name [] $ JSBlock [JSReturn $ JSApp (JSVar $ mkData (mkUnique ctor)) []]
    mkfn name (arg:args) = JSFunction name [arg] $ JSBlock [JSReturn $ mkfn Nothing args]
    mkfn Nothing [] = JSApp (JSVar $ mkData (mkUnique ctor)) (JSVar <$> last . words <$> fields ty)

    fname = Just $ fty (types ty) ++ ' ': dataCtorName;

    fty :: [String] -> String
    fty [] = asDataTy $ mkUnique ctor
    fty [_] = asDataTy $ mkUnique ctor
    fty (_:t:ts) = "fn<" ++ t ++ "," ++ fty ts ++ ">"

iife :: String -> [JS] -> JS
iife v exprs = JSApp (JSFunction Nothing [] (JSBlock $ exprs ++ [JSReturn $ JSVar v])) []

literalToValueJS :: (Functor m, Applicative m, Monad m) => ModuleName -> Literal (Expr Ann) -> SupplyT m JS
literalToValueJS _ (NumericLiteral n) = return $ JSNumericLiteral n
literalToValueJS _ (StringLiteral s) = return $ JSStringLiteral s
literalToValueJS _ (BooleanLiteral b) = return $ JSBooleanLiteral b
literalToValueJS m (ArrayLiteral xs) = JSArrayLiteral <$> mapM (valueToJs m) xs
literalToValueJS m (ObjectLiteral ps) = JSObjectLiteral <$> mapM (sndM (valueToJs m)) ps

-- |
-- Shallow copy an object.
--
extendObj :: (Functor m, Applicative m, Monad m) => JS -> [(String, JS)] -> SupplyT m JS
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
  return $ JSApp (JSFunction Nothing [] block) []

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
bindersToJs :: (Functor m, Applicative m, Monad m) => ModuleName -> [CaseAlternative Ann] -> [JS] -> SupplyT m JS
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
  return $ JSApp (JSFunction name [] (JSBlock (assignments ++ concat jss ++ [JSThrow $ JSStringLiteral "Failed pattern match"])))
                 []
  where
    go :: (Functor m, Applicative m, Monad m) => [String] -> [JS] -> [Binder Ann] -> SupplyT m [JS]
    go _ done [] = return done
    go (v:vs) done' (b:bs) = do
      done'' <- go vs done' bs
      binderToJs m v done'' b
    go _ _ _ = error "Invalid arguments to bindersToJs"

    guardsToJs :: (Functor m, Applicative m, Monad m) => Either [(Guard Ann, Expr Ann)] (Expr Ann) -> SupplyT m [JS]
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
binderToJs :: (Functor m, Applicative m, Monad m) => ModuleName -> String -> [JS] -> Binder Ann -> SupplyT m [JS]
binderToJs _ _ done (NullBinder{}) = return done
binderToJs m varName done (LiteralBinder _ l) =
  literalToBinderJS m varName done l
binderToJs _ varName done (VarBinder _ ident) =
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : done)
binderToJs m varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
  binderToJs m varName done b
binderToJs m varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType _)) _ ctor bs) = do
  js <- go 0 done bs
  return $ case ctorType of
    ProductType -> js
    SumType ->
      [JSIfElse (JSInstanceOf (JSVar varName) (JSVar ctorName))
                (JSBlock js)
                Nothing]
  where
  go :: (Functor m, Applicative m, Monad m) => Integer -> [JS] -> [Binder Ann] -> SupplyT m [JS]
  go _ done' [] = return done'
  go index done' (binder:bs') = do
    argVar <- freshName
    done'' <- go (index + 1) done' bs'
    js <- binderToJs m argVar done'' binder
    return (JSVariableIntroduction argVar (Just (JSAccessor ("value" ++ show index) (JSCast (JSVar ctorName) (JSVar varName)))) : js)
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
  return [JSIfElse (JSBinary GreaterThanOrEqualTo (JSApp (JSAccessor "size" (JSVar varName)) []) (JSNumericLiteral (Left numberOfHeadBinders))) (JSBlock
    ( JSVariableIntroduction tailVar (Just (JSApp (JSVar . drop 1 $ getType varName) [
                                            JSBinary Add (JSApp (JSAccessor "begin" (JSVar varName)) [])
                                                         (JSNumericLiteral (Left numberOfHeadBinders)),
                                            JSApp (JSAccessor "end" (JSVar varName)) []
                                            ])) : js2
    )) Nothing]
  where
  uncons :: [Binder Ann] -> Binder Ann -> ([Binder Ann], Binder Ann)
  uncons acc (ConstructorBinder _ _ ctor' [h, t]) | isCons ctor' = uncons (h : acc) t
  uncons acc tailBinder = (reverse acc, tailBinder)
binderToJs _ _ _ b@(ConstructorBinder{}) =
  error $ "Invalid ConstructorBinder in binderToJs: " ++ show b
binderToJs m varName done (NamedBinder _ ident binder) = do
  js <- binderToJs m varName done binder
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : js)

literalToBinderJS :: (Functor m, Applicative m, Monad m) => ModuleName -> String -> [JS] -> Literal (Binder Ann) -> SupplyT m [JS]
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
  go :: (Functor m, Applicative m, Monad m) => [JS] -> [(String, Binder Ann)] -> SupplyT m [JS]
  go done' [] = return done'
  go done' ((prop, binder):bs') = do
    propVar <- freshName
    done'' <- go done' bs'
    js <- binderToJs m propVar done'' binder
    return (JSVariableIntroduction propVar (Just (accessorString prop (JSVar varName))) : js)
literalToBinderJS m varName done (ArrayLiteral bs) = do
  js <- go done 0 bs
  return [JSIfElse (JSBinary EqualTo (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
  where
  go :: (Functor m, Applicative m, Monad m) => [JS] -> Integer -> [Binder Ann] -> SupplyT m [JS]
  go done' _ [] = return done'
  go done' index (binder:bs') = do
    elVar <- freshName
    done'' <- go done' (index + 1) bs'
    js <- binderToJs m elVar done'' binder
    return (JSVariableIntroduction elVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : js)

isCons :: Qualified ProperName -> Bool
isCons (Qualified (Just mn) ctor) = mn == ModuleName [ProperName C.prim] && ctor == ProperName "Array"
isCons name = error $ "Unexpected argument in isCons: " ++ show name
