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

import Data.Function (on)
import Data.List ((\\), delete, sortBy, isSuffixOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Char (isLower)

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

import Language.PureScript.Types
import Language.PureScript.Pretty.Common
import Language.PureScript.CodeGen.Go
import Debug.Trace

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: (Functor m, Applicative m, Monad m) => Options mode -> Module Ann -> SupplyT m [JS]
moduleToJs opts (Module name imps exps foreigns decls) = do
  let jsImports = map (importToJs opts) . delete (ModuleName [ProperName C.prim]) . (\\ [name]) $ imps
  let foreigns' = mapMaybe (\(_, js, _) -> js) foreigns
  jsDecls <- mapM (bindToJs name True) decls
  let optimized = concatMap (map $ optimize opts) $ catMaybes jsDecls
  return $ [ JSRaw ("package " ++ moduleName)
           , JSRaw ("")
           , JSRaw ("import \"reflect\"")
           , JSRaw ("import \"fmt\"")
           , JSRaw ("")
           ]
             ++ jsImports
             ++ (if isPrelude name then appFnDef
                 else [JSRaw ("import . \"Prelude\"")
                     , JSRaw ("")
                     , JSRaw ("var _ reflect.Value // ignore unused package errors")
                     , JSRaw ("var _ fmt.Formatter //")
                     , JSRaw ("var _ = Prelude." ++ appFn)
                     , JSRaw ("")])
             ++ optimized
             ++ foreigns'
  where
    moduleName = case name of (ModuleName [ProperName "Main"]) -> "main"
                              _ -> unqual $ moduleNameToJs' name
-- |
-- Generates Javascript code for a module import.
--
importToJs :: Options mode -> ModuleName -> JS
importToJs opts mn =
  JSRaw $ "import " ++ (dotsTo '_' (moduleNameToJs' mn)) ++ " \"" ++ (dotsTo '/' (moduleNameToJs' mn)) ++ "\""

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
  return $ JSVariableIntroduction (identToJs ident) (Just js)
  

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
accessorString :: String -> JS -> JS
accessorString prop = JSAccessor $ typeclassPrefix ++ (identToJs $ Ident prop)

mapAccessorString :: String -> JS -> JS
mapAccessorString prop = JSIndexer (JSStringLiteral prop) . withCast anyMap

-- |
-- Generate code in the simplified Javascript intermediate representation for a value or expression.
--
valueToJs :: (Functor m, Applicative m, Monad m) => ModuleName -> Expr Ann -> SupplyT m JS
valueToJs m (Literal _ l) =
  literalToValueJS m l
valueToJs m (Var (_, _, Just (IsConstructor _ 0)) name) =
  return $ qualifiedToJS m id (withSuffix ctorSuffix name)
valueToJs m (Var (_, _, Just (IsConstructor _ _)) name) =
  return $ qualifiedToJS m id (withSuffix ctorSuffix name)
valueToJs m (Accessor _ prop val) =
  case val of
    Var (_, Just (TypeApp (TypeConstructor _) (RCons _ _ _)), _) _ -> mapAccessorString prop <$> valueToJs m val
    _ -> accessorString prop <$> valueToJs m val
valueToJs m (ObjectUpdate _ o ps) = do
  obj <- valueToJs m o
  sts <- mapM (sndM (valueToJs m)) ps
  extendObj obj sts
valueToJs _ e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
  let args = unAbs e
  in return $ JSData' "" (JSBlock $ map decl args)
  where
  unAbs :: Expr Ann -> [Ident]
  unAbs (Abs _ arg val) = arg : unAbs val
  unAbs _ = []
  decl :: Ident -> JS
  decl name = JSVar $ typeclassPrefix ++ identToJs name ++ withSpace anyType
valueToJs m e@(Abs (_, _, Just IsNewtype) arg val) = -- TODO: revisit this
  let args = unAbs e
  in return $ JSData' "" (JSBlock $ map decl args)
  where
  unAbs :: Expr Ann -> [Ident]
  unAbs (Abs _ arg val) = arg : unAbs val
  unAbs _ = []
  decl :: Ident -> JS
  decl name = JSVar . capitalize $ identToJs name ++ withSpace anyType
valueToJs m (Abs (_, t, _) arg val) = do
  ret <- valueToJs m val
  return $ JSFunction Nothing [identToJs arg ++ type' t] (JSBlock [JSReturn ret])
    where
      type' (Just (ForAll _ ty _)) = type' (Just ty)
      type' (Just (ConstrainedType [((Qualified _ (ProperName name)),_)] _)) = withSpace name
      type' _ = ""
valueToJs m e@App{} = do
  let (f, args) = unApp e []
  args' <- mapM (valueToJs m) args
  case f of
    Var (_, _, Just IsNewtype) _ -> return (head args')
    Var (_, _, Just (IsConstructor _ arity)) name | arity == length args ->
      return $ JSApp (JSVar $ withSuffix' ctorSuffix m name) args'
    Var (_, _, Just IsTypeClassConstructor) name ->
      return $ JSInit (JSVar $ unqualName name) args'
    _ -> do fn <- valueToJs m f; return $ JSApp fn args'
  where
  unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
  unApp (App _ val arg) args = unApp val (arg : args)
  unApp other args = (other, args)
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
valueToJs _ (Constructor _ _ (ProperName ctor) 0) =
  return $ JSBlock [ JSData' ctor (JSBlock [])
         , JSFunction (Just $ ctor ++ ctorSuffix) ["_"] (JSBlock [JSReturn (JSInit (JSVar ctor) [])])
         ]
valueToJs _ (Constructor _ _ (ProperName ctor) arity) =
  return $ JSBlock [ makeConstructor ctor arity
         , (go ctor 0 arity [])
         ]
    where
    makeConstructor :: String -> Int -> JS
    makeConstructor ctorName n =
      let args = [ "value" ++ show index | index <- [0..n-1] ]
          body = [ (JSVar $ arg ++ withSpace anyType) | arg <- args ]
      in JSData' ctorName (JSBlock body)
    go :: String -> Int -> Int -> [JS] -> JS
    go pn _ 0 values = JSInit (JSVar pn) (reverse values)
    go pn index n values =
      JSFunction fname ["value" ++ show index]
        (JSBlock [JSReturn (go pn (index + 1) (n - 1) (JSVar ("value" ++ show index) : values))])
      where
        fname = case index of 0 -> Just $ pn ++ ctorSuffix
                              _ -> Nothing

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
qualifiedToJS m f (Qualified (Just m') a)
  | name@(x:xs) <- (identToJs $ f a), isLower x = JSVar . (if m /= m' && not (isPrelude m') then
                                                             (moduleNameToJs m' ++) . ('.' :)
                                                           else id) $ modulePrefix ++ name
qualifiedToJS _ f (Qualified _ a) = JSVar $ identToJs (f a)

-- |
-- Generate code in the simplified Javascript intermediate representation for pattern match binders
-- and guards.
--
bindersToJs :: (Functor m, Applicative m, Monad m) => ModuleName -> [CaseAlternative Ann] -> [JS] -> SupplyT m JS
bindersToJs m binders vals = do
  valNames <- replicateM (length vals) freshName
  let assignments = zipWith JSVariableIntroduction valNames (map Just vals)
  jss <- forM binders $ \(CaseAlternative bs result) -> do
    ret <- guardsToJs result
    go valNames ret bs
  return $ JSApp (JSFunction Nothing [] (JSBlock (assignments ++ concat jss ++ [JSThrow (JSStringLiteral "Failed pattern match")])))
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
      [JSIfElse (JSInstanceOf (JSVar varName) (qualifiedToJS m (Ident . runProperName) ctor))
                (JSBlock js)
                Nothing]
  where
  go :: (Functor m, Applicative m, Monad m) => Integer -> [JS] -> [Binder Ann] -> SupplyT m [JS]
  go _ done' [] = return done'
  go index done' (binder:bs') = do
    argVar <- freshName
    done'' <- go (index + 1) done' bs'
    js <- binderToJs m argVar done'' binder
    return (JSVariableIntroduction argVar (Just (JSAccessor (parens (dtype) ++ "." ++ "value" ++ show index) (JSVar varName))) : js)
  (Qualified dmod (ProperName dname)) = d
  dtype = case dmod of
            Nothing -> dname
            Just dmod' -> if dmod' == m then dname else runModuleName dmod' ++ ('.' : dname)
binderToJs m varName done binder@(ConstructorBinder _ _ ctor _) | isCons ctor = do
  let (headBinders, tailBinder) = uncons [] binder
      numberOfHeadBinders = fromIntegral $ length headBinders
  js1 <- foldM (\done' (headBinder, index) -> do
    headVar <- freshName
    jss <- binderToJs m headVar done' headBinder
    return (JSVariableIntroduction headVar (Just (JSIndexer (JSNumericLiteral (Left index)) (withCast anyList (JSVar varName)))) : jss)) done (zip headBinders [0..])
  tailVar <- freshName
  js2 <- binderToJs m tailVar js1 tailBinder
  return [JSIfElse (JSBinary GreaterThanOrEqualTo (listLen (JSVar varName)) (JSNumericLiteral (Left numberOfHeadBinders))) (JSBlock
    ( JSVariableIntroduction tailVar (Just (JSIndexer (JSVar (show numberOfHeadBinders ++ ":")) (withCast anyList (JSVar varName)))) :
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
  return [JSIfElse (JSBinary EqualTo (listLen (JSVar varName)) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
  where
  go :: (Functor m, Applicative m, Monad m) => [JS] -> Integer -> [Binder Ann] -> SupplyT m [JS]
  go done' _ [] = return done'
  go done' index (binder:bs') = do
    elVar <- freshName
    done'' <- go done' (index + 1) bs'
    js <- binderToJs m elVar done'' binder
    return (JSVariableIntroduction elVar (Just (JSIndexer (JSNumericLiteral (Left index)) (withCast anyList (JSVar varName)))) : js)

isCons :: Qualified ProperName -> Bool
isCons (Qualified (Just mn) ctor) = mn == ModuleName [ProperName C.prim] && ctor == ProperName "Array"
isCons name = error $ "Unexpected argument in isCons: " ++ show name

unqualName :: Qualified Ident -> String
unqualName (Qualified _ (Ident name)) = name
unqualName n = show n

withSuffix :: String -> Qualified Ident -> Qualified Ident
withSuffix suffix (Qualified n (Ident name)) = Qualified n (Ident $ name ++ suffix)

withSuffix' :: String -> ModuleName -> Qualified Ident -> String
withSuffix' suffix m full@(Qualified n (Ident name))
  | n == Just m = name ++ suffix
  | otherwise = show full ++ suffix

isPrelude :: ModuleName -> Bool
isPrelude (ModuleName [ProperName "Prelude"]) = True
isPrelude _ = False
