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

{-# LANGUAGE GADTs, ViewPatterns #-}

module Language.PureScript.CodeGen.JS (
    module AST,
    module Common,
    bindToJs,
    moduleToJs
) where

import Data.List ((\\), delete)
import Data.List (elemIndices, intercalate, intersperse, isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)

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
  let moduleHeader = dataTypes decls ++ map stripImpls optimized
  let moduleBody = map stripDecls optimized
  let exps' = JSObjectLiteral $ map (runIdent &&& JSVar . identToJs) exps
  return $ case optionsAdditional opts of
    MakeOptions -> moduleBody -- ++ [JSAssignment (JSAccessor "exports" (JSVar "module")) exps']
    CompileOptions ns _ _ | not isModuleEmpty ->
      [ JSRaw "#include <functional>\n"
      , JSRaw "#include <memory>\n"
      , JSRaw "#include <iostream>\n"
      , JSRaw "#define data std::shared_ptr\n"
      , JSRaw "#define make_data std::make_shared\n"
      , JSRaw "#define cast *std::dynamic_pointer_cast\n"
      , JSRaw "#define instance_of std::dynamic_pointer_cast\n"
      , JSRaw "\ntemplate <typename T, typename U>\nusing fn = std::function<U(T)>"
      , JSRaw "\n"
      ]
      ++ [JSNamespace (moduleNameToJs name) (moduleHeader)
        , JSRaw "// end of header"]
      ++ [JSNamespace (moduleNameToJs name) (moduleBody)]
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
  return $ JSVariableIntroduction (identToJs ident) (expr js)
  where
    expr js = case js of
                JSFunction orig args sts -> Just (JSFunction (fnName orig (identToJs ident)) args sts)
                _ -> Just js

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
valueToJs m (Var (_, _, _, Just (IsConstructor _ _)) name) =
  return $ JSAccessor "create" $ qualifiedToJS m id name
valueToJs m (Accessor _ prop val) =
  accessorString prop <$> valueToJs m val
valueToJs m (ObjectUpdate _ o ps) = do
  obj <- valueToJs m o
  sts <- mapM (sndM (valueToJs m)) ps
  extendObj obj sts
valueToJs m e@(Abs (_, _, _, Just IsTypeClassConstructor) _ val) = return noOp
  -- let fs = fnInfo val []
  -- return $ JSBlock' [] (map mkFn fs)
  -- where
  -- unAbs :: Expr Ann -> [Ident]
  -- unAbs (Abs _ arg val) = arg : unAbs val
  -- unAbs _ = []
  -- assign :: Ident -> JS
  -- assign name = JSAssignment (accessorString (runIdent name) (JSVar "this"))
  --                            (var name)
  --
  -- fnInfo :: Expr Ann -> [(Ident, Maybe T.Type)] -> [(Ident, Maybe T.Type)]
  -- fnInfo (Abs (_, _, ty, _) name val) fs = fnInfo val ((name, ty) : fs)
  -- fnInfo _ fs = fs
  --
  -- mkFn :: (Ident, Maybe T.Type) -> JS
  -- mkFn (_, Nothing) = noOp
  -- mkFn (ident, ty) = JSVariableIntroduction (identToJs ident)
  --                                           (Just $ JSFunction (annotatedName) [fnArgStr ty] noOp)
  --   where annotatedName = Just $ templTypes ty ++ fnRetStr ty ++ ' ' : identToJs ident
valueToJs m (Abs (_, _, (Just (T.ForAll _ (T.ConstrainedType _ _) _)), _) _ _) = return noOp
valueToJs m (Abs (_, _, (Just (T.ConstrainedType ts _)), _) _ val)
    | (Abs (_, _, t, _) _ val') <- val, Nothing <- t = valueToJs m (dropAbs (length ts - 2) val') -- TODO: confirm '-2'
    | otherwise = valueToJs m val
    where
      dropAbs :: Int -> Expr Ann -> Expr Ann
      dropAbs n (Abs _ _ ann) | n > 0 = dropAbs (n-1) ann
      dropAbs _ a = a

valueToJs m (Abs ann arg val) = do
  ret <- valueToJs m val
  return $ JSFunction (Just annotatedName) [fnArgStr m ty ++ ' ' : identToJs arg] (JSBlock [JSReturn ret])
  where
    ty = case ann of (_, _, t, _) -> t
                     _ -> Nothing
    annotatedName = templTypes m ty ++ fnRetStr m ty
valueToJs m e@App{} = do
  let (f, args) = unApp e []
  args' <- mapM (valueToJs m) (filter (not . typeinst) args)
  let tci = instanceJs $ filter typeinst args
  case f of
    Var (_, _, _, Just IsNewtype) _ -> return (head args')
    Var (_, _, _, Just (IsConstructor _ arity)) name | arity == length args ->
      return $ JSApp (JSVar . mkManaged $ qualifiedToStr m id name ++ ttype) args'
    Var (_, _, ty, Just IsTypeClassConstructor) name ->
      return $ JSNamespace [] (map toVarDecl (zip (names ty) args'))
    _ -> flip (foldl (\fn a -> JSApp fn [a])) args' <$> do fn <- valueToJs m f
                                                           return $ instfn tci fn
  where
  unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
  unApp (App (_, _, Nothing, _) val arg@(Var _ (Qualified Nothing _))) args = unApp val args
  unApp (App (_, _, Just dty, _) val arg) args = unApp val (arg : args)
  unApp other args = (other, args)

  names ty = map fst (fst . T.rowToList $ fromMaybe T.REmpty ty)
  toVarDecl :: (String, JS) -> JS
  toVarDecl (nm, js) | JSFunction _ _ _ <- js, C.__superclass_ `isPrefixOf` nm = noOp
  toVarDecl (nm, js) =
    JSVariableIntroduction (identToJs $ Ident nm)
                           (Just $ case js of
                                     JSFunction orig ags sts -> JSFunction (fnName orig nm) ags sts
                                     _ -> js)
  typeinst :: Expr Ann -> Bool
  typeinst (Var (_, _, Nothing, Nothing) (Qualified (Just _) _)) = True
  typeinst _ = False

  instanceJs :: [Expr Ann] -> [JS]
  instanceJs [Var (_, _, Nothing, Nothing) (Qualified (Just _) ident)] = [JSVar $ identToJs ident]
  instanceJs _ = []

  instfn :: [JS] -> JS -> JS
  instfn [JSVar inst] (JSVar name)
    | ':' `elem` name = JSVar . intercalate "::" $ init parts ++ (inst : tail parts)
    | otherwise = JSVar (inst ++ "::" ++ name)
    where
      parts = words $ map (\c -> case c of ':' -> ' '
                                           _ -> c) name
  instfn _ js = js

  ttype
    | (App (_, _, Just dty, _) _ _) <- e, tstr@('[':_:_:_) <- typestr m dty = '<' : (drop 1 $ getType tstr) ++ ">"
    | otherwise = []

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
valueToJs _ (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
  return $ JSVariableIntroduction ctor (Just $
              JSObjectLiteral [("create",
                JSFunction Nothing ["value"]
                  (JSBlock [JSReturn $ JSVar "value"]))])
-- valueToJs _ (Constructor _ _ (ProperName ctor) 0) =
--   return $ iife ctor [ JSFunction (Just ctor) [] (JSBlock [])
--          , JSAssignment (JSAccessor "value" (JSVar ctor))
--               (JSUnary JSNew $ JSApp (JSVar ctor) []) ]
-- valueToJs _ (Constructor _ _ (ProperName ctor) arity) =
--   return $ iife ctor [ makeConstructor ctor arity
--          , JSAssignment (JSAccessor "create" (JSVar ctor)) (go ctor 0 arity [])
--          ]
--     where
--     makeConstructor :: String -> Int -> JS
--     makeConstructor ctorName n =
--       let args = [ "value" ++ show index | index <- [0..n-1] ]
--           body = [ JSAssignment (JSAccessor arg (JSVar "this")) (JSVar arg) | arg <- args ]
--       in JSFunction (Just ctorName) args (JSBlock body)
--     go :: String -> Int -> Int -> [JS] -> JS
--     go pn _ 0 values = JSUnary JSNew $ JSApp (JSVar pn) (reverse values)
--     go pn index n values =
--       JSFunction Nothing ["value" ++ show index]
--         (JSBlock [JSReturn (go pn (index + 1) (n - 1) (JSVar ("value" ++ show index) : values))])
--
-- iife :: String -> [JS] -> JS
-- iife v exprs = JSApp (JSFunction Nothing [] (JSBlock $ exprs ++ [JSReturn $ JSVar v])) []

valueToJs m (Constructor (_, _, ty, _) typ (ProperName ctor) arity) =
    return $ JSData ctor typename (fields ty) (JSVariableIntroduction [] $ Just $ mkfn fname (cleanType <$> fields ty))
  where
    typename = runProperName typ

    fields :: Maybe T.Type -> [String]
    fields ty = map (\(t,n) -> t ++ ' ' : ("value" ++ show n)) $ zip (types ty) [0..]

    types :: Maybe T.Type -> [String]
    types Nothing = []
    types (Just (T.RCons _ ty row)) = (typestr m ty) : types (Just row)
    types (Just T.REmpty) = []

    mkfn :: Maybe String -> [String] -> JS
    mkfn name@(Just _) [] = JSFunction name [] $ JSBlock [JSReturn $ JSApp (JSVar $ mkManaged ctor) []]
    mkfn name (arg:args) = JSFunction name [arg] $ JSBlock [JSReturn $ mkfn Nothing args]
    mkfn Nothing [] = JSApp (JSVar $ mkManaged ctor) (JSVar <$> last . words <$> fields ty)

    fname = Just $ fty (types ty) ++ " create";

    fty :: [String] -> String
    fty [] = managedTy ctor
    fty [_] = managedTy ctor
    fty (_:t:ts) = "fn<" ++ t ++ "," ++ fty ts ++ ">"


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
  jss <- forM binders $ \(CaseAlternative bs result) -> do
    ret <- guardsToJs result
    go valNames ret bs
  return $ JSApp (JSFunction Nothing [] (JSBlock (assignments ++ concat jss ++ [JSThrow $ JSUnary JSNew $ JSApp (JSVar "Error") [JSStringLiteral "Failed pattern match"]])))
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
  ctorName = qualifiedToStr m (Ident . runProperName) ctor
          ++ case getSpecialization varName of
               [] -> []
               _ -> "<" ++ getSpecialization varName ++ ">"
binderToJs m varName done binder@(ConstructorBinder _ _ ctor _) | isCons ctor = do
  let (headBinders, tailBinder) = uncons [] binder
      numberOfHeadBinders = fromIntegral $ length headBinders
  js1 <- foldM (\done' (headBinder, index) -> do
    headVar <- freshName
    jss <- binderToJs m headVar done' headBinder
    return (JSVariableIntroduction headVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : jss)) done (zip headBinders [0..])
  tailVar <- freshName
  js2 <- binderToJs m tailVar js1 tailBinder
  return [JSIfElse (JSBinary GreaterThanOrEqualTo (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left numberOfHeadBinders))) (JSBlock
    ( JSVariableIntroduction tailVar (Just (JSApp (JSAccessor "slice" (JSVar varName)) [JSNumericLiteral (Left numberOfHeadBinders)])) :
      js2
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

noOp :: JS
noOp = JSRaw []

typestr :: ModuleName -> T.Type -> String
typestr _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Number")))  = "int"
typestr _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "String")))  = "string"
typestr _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Boolean"))) = "bool"
typestr _ (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               T.REmpty) _)
                 = error "Need to supprt func() T"
typestr m (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               a) b)
                 = "fn<" ++ typestr m a ++ "," ++ typestr m b ++ ">"
typestr m (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Array")))
             a)
               = ("std::vector<" ++ typestr m a ++ ">")
-- typestr m (T.TypeApp a@(T.TypeConstructor _) b@(T.TypeConstructor _)) = typestr m a ++ addType (typestr m b)
typestr m (T.TypeApp a@(T.TypeConstructor _) b) = typestr m a ++ addType (typestr m b)
typestr m (T.TypeApp a b) = typestr m a ++ addType (typestr m b)
typestr m (T.ForAll _ ty _) = typestr m ty
typestr _ (T.Skolem nm _ _) = '\'' : nm
typestr _ (T.TypeVar nm) = '\'' : nm
typestr m (T.TypeConstructor typ) = let brk = map (\c -> if c=='.' then ' ' else c) in
                                    managedTy . intercalate "::" . words . brk $ qualifiedToStr m (Ident . runProperName) typ
typestr _ t = "T"

fnArgStr :: ModuleName -> Maybe T.Type -> String
fnArgStr m (Just ((T.TypeApp
                    (T.TypeApp
                      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                      (T.TypeConstructor a)) _)))
                         = managedTy $ qualifiedToStr m (Ident . runProperName) a
fnArgStr m (Just ((T.TypeApp
                    (T.TypeApp
                      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                       a) _)))
                         = cleanType $ typestr m a
fnArgStr _ _ = []

fnRetStr :: ModuleName -> Maybe T.Type -> String
fnRetStr m (Just ((T.TypeApp
                    (T.TypeApp
                      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                       _) (T.TypeConstructor b))))
                         = managedTy $ qualifiedToStr m (Ident . runProperName) b
fnRetStr m (Just ((T.TypeApp
                    (T.TypeApp
                      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                       _) b)))
                         = cleanType $ typestr m b
fnRetStr _ _ = []

fnName :: Maybe String -> String -> Maybe String
fnName Nothing name = Just name
fnName (Just t) name = Just (t ++ ' ' : (identToJs $ Ident name))

cleanType :: String -> String
cleanType = filter (/= '\'') . rmType

templTypes :: ModuleName -> Maybe T.Type -> String
templTypes m (Just t) =
  let s = typestr m t
      ss = (takeWhile isAlphaNum . flip drop s) <$> (map (+1) . elemIndices '\'' $ s) in
      if null ss then "" else intercalate ", " (map ("class " ++) . nub . sort $ ss) ++ "|"
templTypes _ _ = ""

stripImpls :: JS -> JS
stripImpls (JSNamespace name bs) = JSNamespace name (map stripImpls bs)
stripImpls (JSComment c e) = JSComment c (stripImpls e)
stripImpls (JSVariableIntroduction var (Just expr)) = JSVariableIntroduction var (Just $ stripImpls expr)
stripImpls (JSFunction fn args _) = JSFunction fn args noOp
stripImpls dat@(JSData _ _ _ _) = dat
stripImpls _ = noOp

stripDecls :: JS -> JS
stripDecls (JSVariableIntroduction var (Just expr)) = JSVariableIntroduction var (Just $ stripDecls expr)
stripDecls dat@(JSData _ _ _ _) = noOp
stripDecls js = js

dataTypes :: [Bind Ann] -> [JS]
dataTypes = map (JSVar . mkClass) . nub . filter (not . null) . map dataType
  where
    mkClass :: String -> String
    mkClass s = templateDecl ++ "struct " ++ rmType s ++ " { virtual ~" ++ rmType s ++ "(){} }"
      where
        templateDecl
          | t@('[':_:_:_) <- drop 1 $ getType s = "template" ++ '<' : intercalate ", " (("typename " ++) <$> read t) ++ "> "
          | otherwise = []

dataType :: Bind Ann -> String
dataType (NonRec _ (Constructor (_, _, _, _) name _ _)) = runProperName name
dataType _ = []

qualifiedToStr :: ModuleName -> (a -> Ident) -> Qualified a -> String
qualifiedToStr _ f (Qualified (Just (ModuleName [ProperName mn])) a) | mn == C.prim = runIdent $ f a
qualifiedToStr m f (Qualified (Just m') a) | m /= m' = moduleNameToJs m' ++ "::" ++ identToJs (f a)
qualifiedToStr _ f (Qualified _ a) = identToJs (f a)

managedTy :: String -> String
managedTy t = "data<" ++ t ++ ">"

mkManaged :: String -> String
mkManaged t = "make_data<" ++ t ++ ">"

addType :: String -> String
addType t = '@' : t

getType :: String -> String
getType = dropWhile (/='@')

getSpecialization :: String -> String
getSpecialization = drop 1 . getType . drop 1 . getType

rmType :: String -> String
rmType = takeWhile (/='@')
