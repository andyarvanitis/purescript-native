-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Types
-- Copyright   :  (c) Andy Arvanitis 2015
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Types for native C++11 translation
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.CodeGen.Cpp.Types where

import Data.List
import Data.Char
import Data.Maybe
import Data.Function (on)
import Data.Data
import Control.Monad (liftM2)
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.CodeGen.Cpp.Common
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Types as T

import Debug.Trace

data Type = Native String [Type]
          | Function Type Type
          | List Type
          | Map [(String,Type)]
          | Template String [Type]
          | TypeConstructor String Type
          | DeclType String
          | EffectFunction Type
          deriving (Show, Data, Typeable)

instance Eq Type where
  (Native t ts) == (Native t' ts') = t == t' && ts == ts'
  (Function a b) == (Function a' b') = a == a' && b == b'
  (List a) == (List a') = a == a'
  (Map ms) == (Map ms') = ms == ms'
  (Template t ts) == (Template t' ts') = cap t == cap t' && ts == ts'
    where
    cap :: String -> String
    cap (c:cs) = toUpper c : cs
    cap s = s
  (TypeConstructor s a) == (TypeConstructor s' a') = a == a' && s == s'
  (DeclType e) == (DeclType e') = e == e'
  (EffectFunction b) == (EffectFunction b') = b == b'
  _ == _ = False
  a /= b = not (a == b)

-- |
-- Value C++11 qualifiers
--
data CppQualifier
  -- |
  -- Struct, class, file, etc. static
  --
  = CppStatic
  -- |
  -- Virtual function
  --
  | CppVirtual
  -- |
  -- C++ class constructor function
  --
  | CppConstructor
  -- |
  -- C++ class destructor function
  --
  | CppDestructor
  -- |
  -- C++ class constructor/destructor, default implementation
  --
  | CppDefault
  -- |
  -- C++ class constructor/destructor, deleted
  --
  | CppDelete
  -- |
  -- Inline function
  --
  | CppInline
  -- |
  -- Constant expression
  --
  | CppConstExpr
  -- |
  -- Mutable expression
  --
  | CppMutable
  -- |
  -- Template partial specialization
  --
  | CppTemplSpec
  deriving (Show, Eq, Data, Typeable)

-- |
-- C++ lambda capture list
--
data CppCaptureType
  = CppCaptureAll
  deriving (Show, Eq, Data, Typeable)

runType :: Type -> String
runType (Native t []) = t
-- TODO: make this a separate type
runType (Native "fn_" [t]) = "fn_<" ++ runType t ++ ">::template _"
runType (Native [] ts) = intercalate "," (map runType ts)
runType (Native t ts) = t ++ '<' : intercalate "," (map runType ts) ++ ">"
runType tt@(Function (Template [] []) (Template [] [])) = typeName tt
runType tt@(Function a b) = typeName tt ++ '<' : runType a ++ "," ++ runType b ++ ">"
runType tt@(EffectFunction (Template [] [])) = typeName tt
runType tt@(EffectFunction b) = typeName tt ++ '<' : runType b ++ ">"
runType tt@(List t) | t'@(_:_) <- runType t = typeName tt ++ '<' : t' ++ ">"
                    | otherwise = typeName tt
runType tt@(Map _) = typeName tt
runType tt@(TypeConstructor mn t) = mn ++ "::" ++ typeName tt ++ runType t ++ "::template _"
runType tt@(DeclType s) = typeName tt ++ '(' : s ++ ")"
runType tt@(Template t []) = typeName tt ++ capitalize t
runType (Template t ts) = runType (Template t []) ++ '<' : (intercalate "," $ map runType ts) ++ ">"

typeName :: Type -> String
typeName Function{} = "fn"
typeName EffectFunction{} = "eff_fn"
typeName List{} = "list"
typeName Map{} = "any_map"
typeName TypeConstructor{} = "type::"
typeName DeclType{} = "decltype"
typeName _ = ""

everywhereOnTypes :: (Type -> Type) -> Type -> Type
everywhereOnTypes f = go
  where
  go (Function t1 t2) = f (Function (go t1) (go t2))
  go (List t) = f (List (go t))
  go (Map ts) = f (Map (map (\(n,t) -> (n, go t)) ts))
  go (Template s ts) = f (Template s (map go ts))
  go (EffectFunction t) = f (EffectFunction (go t))
  go other = f other


everythingOnTypes :: (r -> r -> r) -> (Type -> r) -> Type -> r
everythingOnTypes (<>) f = go
  where
  go t@(Native _ tys) = foldl (<>) (f t) (map go tys)
  go t@(Function t1 t2) = f t <> go t1 <> go t2
  go t@(List ty) = f t <> go ty
  go t@(Map tys) = foldl (<>) (f t) (map (go . snd) tys)
  go t@(Template _ tys) = foldl (<>) (f t) (map go tys)
  go t@(EffectFunction ty) = f t <> go ty
  go other = f other

mktype :: ModuleName -> T.Type -> Maybe Type

mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Number")))  = Just $ Native "double" []
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "String")))  = Just $ Native "string" []
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Boolean"))) = Just $ Native "bool" []
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Int")))     = Just $ Native "int" []
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Char")))    = Just $ Native "char" []

mktype _ (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               T.REmpty) _) = error "Need to supprt func() T"

mktype m (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               a) b) = do
                 a' <- mktype m a
                 b' <- mktype m b
                 return (Function a' b')

-- This covers ((->) r)
mktype m (T.TypeApp
           (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
            r) | Just r' <- mktype m r  = Just $ Native "fn_" [r']

mktype m (T.TypeApp a
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               b)) | Just a' <- mktype m a, Just b' <- mktype m b = Just (Function a' b')

mktype m (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Array")))
             a) = mktype m a >>= Just . List

mktype _ (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Object")))
             T.REmpty) = Just (Map [])

mktype m (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Object")))
             t@(T.RCons _ _ _)) = mktype m t

mktype m (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName ([ProperName "Control",
                                                                ProperName "Monad",
                                                                ProperName "Eff"]))) (ProperName "Eff")))
               _) e) | Just t <- mktype m e = Just $ EffectFunction t

mktype m (T.TypeApp _
           (T.TypeApp
             (T.TypeConstructor (Qualified (Just (ModuleName ([ProperName "Control",
                                                               ProperName "Monad",
                                                               ProperName "Eff"]))) (ProperName "Eff")))
             e)) | Just t <- mktype m e = Just $ EffectFunction t

mktype _ (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName ([ProperName "Control",
                                                              ProperName "Monad",
                                                              ProperName "Eff"]))) (ProperName "Eff")))
            _) = Just $ Native (typeName (EffectFunction (Template [] []))) []

mktype m app@T.TypeApp{}
  | (name, tys@(_:_)) <- tyapp app [] = Just $ Template (identToCpp $ Ident name) tys
  where
    tyapp :: T.Type -> [Type] -> (String, [Type])
    tyapp (T.TypeApp (T.TypeVar name) b) ts | Just b' <- mktype m b = (identToCpp $ Ident name, b':ts)
    tyapp (T.TypeApp (T.Skolem name _ _) b) ts | Just b' <- mktype m b = (identToCpp $ Ident name, b':ts)
    tyapp (T.TypeApp inner@(T.TypeApp _ _) t) ts | Just t' <- mktype m t = tyapp inner (t':ts)
    tyapp _ _ = ([],[])

mktype m (T.TypeApp T.Skolem{} b) = mktype m b

-- TODO: Need to review this due to refactoring
mktype m app@(T.TypeApp a b)
  | (T.TypeConstructor _) <- a, [t] <- dataCon m a, Just b' <- mktype m b = Just $ Native (runType t) [b']
  | (T.TypeConstructor _) <- a, [t] <- dataCon m app = Just $ Native (runType t) []
  | (T.TypeConstructor _) <- a, (t:ts) <- dataCon m app = Just $ Native (runType t) ts
  | (T.TypeConstructor _) <- b, [t] <- dataCon m app = Just $ Native (runType t) []
  | (T.TypeConstructor _) <- b, (t:ts) <- dataCon m app = Just $ Native (runType t) ts
  | (T.TypeApp _ _) <- a, hasCtor a = let (ctor, ps) = getCtor app [] in Just (Native ctor ps)
    where
    hasCtor :: T.Type -> Bool
    hasCtor (T.TypeApp a' _) = hasCtor a'
    hasCtor T.TypeConstructor{} = True
    hasCtor _ = False
    getCtor :: T.Type -> [Type] -> (String, [Type])
    getCtor (T.TypeApp a' b') ts = getCtor a' $ (maybeToList $ mktype m b') ++ ts
    getCtor (T.TypeConstructor c) ts = (qualifiedToStr m (Ident . runProperName) c, ts)
    getCtor t _ = error $ "getCtor: " ++ show t
  -- | (T.TypeApp _ _) <- a, (t:ts) <- dataCon m app = Just $ Native (runType t) ts

mktype m (T.ForAll _ ty _) = mktype m ty
mktype _ (T.Skolem name _ _) = Just $ Template (identToCpp $ Ident name) []
mktype _ (T.TypeVar name) = Just $ Template (identToCpp $ Ident name) []
mktype _ (T.TUnknown n) = Just $ Template ('T' : show n) []
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function"))) =
  Just $ Native (typeName (Function (Template [] []) (Template [] []))) []
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Array"))) =
  Just $ Native (typeName (List (Template [] []))) []
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName ([ProperName "Control",
                                                           ProperName "Monad",
                                                           ProperName "Eff"]))) (ProperName "Eff"))) =
  Just $ Native (typeName (EffectFunction (Template [] []))) []
mktype m a@(T.TypeConstructor _) = Just $ Native (qualDataTypeName m a) []
mktype m (T.ConstrainedType _ ty) = mktype m ty

mktype m r@(T.RCons _ _ _)
  | (rs, _) <- T.rowToList r = Just (Map (rowPairs rs))
  where
  rowPairs :: [(String, T.Type)] -> [(String, Type)]
  rowPairs rs = map (\(n, t) -> (n, fromJust t)) $ filter (isJust . snd) (map (\(n,t) -> (n, mktype m t)) rs)

mktype _ T.REmpty = Nothing
mktype _ b = error $ "Unknown type: " ++ show b

mkTemplate :: String -> Type
mkTemplate s = Template s []

isTemplate :: Type -> Bool
isTemplate Template{} = True
isTemplate _ = False

typestr :: ModuleName -> T.Type -> String
typestr m t = maybe [] runType (mktype m t)

argtype :: Maybe Type -> Maybe Type
argtype (Just (Function a _)) = Just a
argtype _ = Nothing

rettype :: Maybe Type -> Maybe Type
rettype (Just (Function _ b)) = Just b
rettype (Just (EffectFunction b)) = Just b
rettype _ = Nothing

fnTypesN :: Int -> Type -> [Type]
fnTypesN 0 t = [t]
fnTypesN n (Function a b) = a : types (n - 1) b
  where
  types :: Int -> Type -> [Type]
  types 0 t = [t]
  types n' (Function a' b') = a' : types (n' - 1) b'
  types _ _ = []
fnTypesN _ _ = []

templateVars :: Type -> [Type]
templateVars = nub . sortBy (compare `on` name) . go
  where
  go :: Type -> [Type]
  go (Template p ts) = concatMap go ts ++ [Template p []]
  go (Function a b) = go a ++ go b
  go (EffectFunction b) = go b
  go (List a) = go a
  go (Native _ ts) = concatMap go ts
  go _ = []
  name :: Type -> String
  name (Template n _) = n
  name _ = []

templparams :: Type -> [(String, Int)]
templparams (Template p ts) = concatMap templparams ts ++ [(runType (Template p []), length ts)]
templparams (Function a b) = templparams a ++ templparams b
templparams (EffectFunction b) = templparams b
templparams (List a) = templparams a
templparams (Native _ ts) = concatMap templparams ts
templparams _ = []

templparams' :: Maybe Type -> [(String, Int)]
templparams' = sortBy (compare `on` fst) . nub . maybe [] templparams

dataCon :: ModuleName -> T.Type -> [Type]
dataCon m (T.TypeApp a b) = (dataCon m a) ++ (dataCon m b)
dataCon m a@(T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) _)) =
  maybe [] (replicate 1) (mktype m a)
dataCon m a@(T.TypeConstructor _) = [Native (qualDataTypeName m a) []]
dataCon m a = maybe [] (replicate 1) (mktype m a)

getDataType :: String -> Type -> Maybe Type
getDataType name (Function _ b) = getDataType name b
getDataType name t@(Native name' _) | name' == name = Just t
getDataType name (Native _ ts) = Just (Native name ts)
getDataType _ _ = Nothing

getDataTypeArgs :: Type -> [Type]
getDataTypeArgs (Native _ []) = []
getDataTypeArgs (Native _ ts) = ts
getDataTypeArgs _ = []

-- TODO: this should be moved out of this module
--
qualifiedToStr :: ModuleName -> (a -> Ident) -> Qualified a -> String
qualifiedToStr _ f (Qualified (Just (ModuleName [ProperName mn])) a) | mn == C.prim = runIdent $ f a
qualifiedToStr m f (Qualified (Just m') a) | m /= m' = moduleNameToCpp m' ++ "::" ++ identToCpp (f a)
qualifiedToStr _ f (Qualified _ a) = identToCpp (f a)

qualDataTypeName :: ModuleName -> T.Type -> String
qualDataTypeName m (T.TypeConstructor typ) = intercalate "::" . words $ brk tname
  where
    tname = qualifiedToStr m (Ident . runProperName) typ
    brk = map (\c -> if c=='.' then ' ' else c)
qualDataTypeName _ _ = []

runQualifier :: CppQualifier -> String
runQualifier CppStatic = "static"
runQualifier CppInline = "inline"
runQualifier CppVirtual = "virtual"
runQualifier CppConstructor = ""
runQualifier CppDestructor = "~"
runQualifier CppDefault = ""
runQualifier CppDelete = ""
runQualifier CppConstExpr = "constexpr"
runQualifier CppMutable = ""
runQualifier CppTemplSpec = "template <>"

runCaptureType :: CppCaptureType -> String
runCaptureType CppCaptureAll = "="

templateMappings :: (Type, Type) -> [(Type, Type)]
templateMappings = sortBy (compare `on` runType . fst) . nub . go []
  where
    go :: [(Type, Type)] -> (Type, Type) -> [(Type, Type)]
    go args (a@(Template _ []), a') = args ++ [(a, a')]
    go args (Template p ts, Template p' ts') =
      args ++ ((Template p []), (Template p' [])) : concatMap (go []) (zip ts ts')
    go args (Template t [a, b], Function a' b') =
      args ++ (Template t [], Function anytype anytype) : (go [] (a, a')) ++ (go [] (b, b'))
    go args (Template t [a, b], Native "fn" []) =
      args ++ [(Template t [], Function a b)]
    go args (Template t [b], EffectFunction b') =
      args ++ (Template t [], EffectFunction anytype) : (go [] (b, b'))
    go args (Template t [a], List a') =
      args ++ (Template t [], List anytype) : (go [] (a, a'))
    go args (Template t ts, Native t' ts') | length ts == length ts' =
      args ++ (Template t [], Native t' []) : concatMap (go []) (zip ts ts')
    go args (Template t _, Native t' []) =
      args ++ [(Template t [], Native t' [])]
    go args (a@DeclType{}, a') = args ++ [(a, a')]
    go args (Function a b, Function a' b') = args ++ (go [] (a, a')) ++ (go [] (b, b'))
    go args (EffectFunction b, EffectFunction b') = go args (b, b')
    go args (Native _ ts@(_:_), Native _ ts'@(_:_)) = args ++ concatMap (go []) (zip ts ts')
    go args (Native t ts, Map ts') | length ts == length ts' = args ++ concatMap (go []) (zip ts (map snd ts'))
    go args (List t, List t') = go args (t, t')
    go args (Map ms, Map ms') = args ++ concatMap (go []) (zip (map snd ms) (map snd ms'))
    go args (Native _ _, Native _ _) = args
      -- | t == t' = args
      -- | otherwise = error ("Type conflict! " ++ t ++ " ; " ++ t')
    go args (t1', t2') = trace ("Mismatched type structure! " ++ show t1' ++ " ; " ++ show t2') args

templateReplacements :: (Type, Type) -> [(Type, Type)]
templateReplacements = filter (\(a,b) -> a /= b) . templateMappings

templateSpecs :: Maybe Type -> Maybe Type -> [Type]
templateSpecs tyDecl tyExpr = map snd $ maybe [] templateMappings (liftM2 (,) tyDecl tyExpr)

templateFromKind :: (String, Maybe Kind) -> (String, Int)
templateFromKind (name, Just Star) = (capitalize name, 0)
templateFromKind (name, Just f@(FunKind _ _)) = (capitalize name, numFunKindArgs f)
  where
  numFunKindArgs :: Kind -> Int
  numFunKindArgs = everythingOnKinds (+) go
    where
    go :: Kind -> Int
    go (FunKind _ _) = 1
    go _ = 0
templateFromKind k = error $ show "Unsupported kind! (" ++ show k ++ ")"

addTemplateDefaults :: [(String, Int)] -> [(String, Int)]
addTemplateDefaults = map addDefault
  where
  addDefault ::  (String, Int) -> (String, Int)
  addDefault (name, 0) = (name ++ " = void", 0)
  addDefault (name, n) = (name ++ " = void" ++ show n, n)

remTemplateDefaults :: [(String, Int)] -> [(String, Int)]
remTemplateDefaults = map remDefault
  where
  remDefault ::  (String, Int) -> (String, Int)
  remDefault (name, 0) = (takeWhile isAlphaNum name, 0)
  remDefault (name, n) = (takeWhile isAlphaNum name, n)

anytype :: Type
anytype = Template [] []

capitalize :: String -> String
capitalize = map toUpper
