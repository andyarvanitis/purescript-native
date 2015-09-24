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
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.CodeGen.Cpp.Common
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Types as T

import Debug.Trace

data Type = Primitive String
          | Native String [Type]
          | Function Type Type
          | Array Type
          | Map [(String, Type)]
          | Template String [Type]
          | TypeConstructor String Type
          | DeclType String
          | EffectFunction Type
          | AutoType
          | AnyType
          | AnyTypeRef
          deriving (Show, Data, Typeable)

instance Eq Type where
  (Primitive t) == (Primitive t') = t == t'
  (Native t ts) == (Native t' ts') = t == t' && ts == ts'
  (Function a b) == (Function a' b') = a == a' && b == b'
  (Array a) == (Array a') = a == a'
  (Map ms) == (Map ms') = ms == ms'
  (Template t ts) == (Template t' ts') = makeUnique t == makeUnique t' && ts == ts'
  (TypeConstructor s a) == (TypeConstructor s' a') = a == a' && s == s'
  (DeclType e) == (DeclType e') = e == e'
  (EffectFunction b) == (EffectFunction b') = b == b'
  AutoType == AutoType = True
  AnyType == AnyType = True
  AnyTypeRef == AnyTypeRef = True
  _ == _ = False
  a /= b = not (a == b)

-- |
-- Template parameter details
--
type TemplateInfo = (String, Int)

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
  -- Extern value
  --
  | CppExtern
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
runType (Primitive t) = t
runType (Native t []) = t
-- TODO: make this a separate type
runType (Native "fn_" [t]) = "fn_<" ++ runType t ++ ">::template _"
runType (Native [] ts) = intercalate "," (map runType ts)
runType (Native t ts) = t ++ '<' : intercalate "," (map runType ts) ++ ">"
runType tt@(Function (Template [] []) (Template [] [])) = typeName tt
runType tt@(Function a b) = typeName tt ++ '<' : runType a ++ "," ++ runType b ++ ">"
runType tt@(EffectFunction (Template [] [])) = typeName tt
runType tt@(EffectFunction b) = typeName tt ++ '<' : runType b ++ ">"
runType tt@(Array t) | t'@(_:_) <- runType t = typeName tt ++ '<' : t' ++ ">"
                    | otherwise = typeName tt
runType (Map []) = "nullptr_t"
runType tt@(Map fields) = typeName tt ++ "<" ++ (intercalate ", " $ map runField fields) ++ ">"
  where
  runField :: (String, Type) -> String
  runField (name, typ) = show name ++ "_key" ++ ", const " ++ runType typ
runType (TypeConstructor [] t) = "typename " ++ runType t ++ "::_"
runType tt@(TypeConstructor mn t) = mn ++ "::" ++ typeName tt ++ runType t ++ "::template _"

runType tt@(DeclType s) = typeName tt ++ '(' : s ++ ")"
runType tt@(Template t []) = typeName tt ++ makeUnique t
runType (Template t ts) = runType (Template t []) ++ '<' : (intercalate "," $ map runType ts) ++ ">"
runType AutoType = "auto"
runType AnyType = "any"
runType AnyTypeRef = "const " ++ runType AnyType ++ "&"

autoType ::T.Type
autoType = T.TypeConstructor (Qualified Nothing (ProperName "auto"))

typeName :: Type -> String
typeName Function{} = "fn"
typeName EffectFunction{} = "eff_fn"
typeName Array{} = "array"
typeName Map{} = "cmap"
typeName TypeConstructor{} = "type::"
typeName DeclType{} = "decltype"
typeName AutoType = ""
typeName AnyType = ""
typeName AnyTypeRef = ""
typeName _ = ""

everywhereOnTypes :: (Type -> Type) -> Type -> Type
everywhereOnTypes f = go
  where
  go (Native s ts) = f (Native s (map go ts))
  go (Function t1 t2) = f (Function (go t1) (go t2))
  go (Array t) = f (Array (go t))
  go (Map ts) = f (Map (map (\(n,t) -> (n, go t)) ts))
  go (Template s ts) = f (Template s (map go ts))
  go (TypeConstructor s t) = f (TypeConstructor s (go t))
  go (EffectFunction t) = f (EffectFunction (go t))
  go other = f other

everythingOnTypes :: (r -> r -> r) -> (Type -> r) -> Type -> r
everythingOnTypes (<>) f = go
  where
  go t@(Native _ tys) = foldl (<>) (f t) (map go tys)
  go t@(Function t1 t2) = f t <> go t1 <> go t2
  go t@(Array ty) = f t <> go ty
  go t@(Map tys) = foldl (<>) (f t) (map (go . snd) tys)
  go t@(Template _ tys) = foldl (<>) (f t) (map go tys)
  go t@(TypeConstructor _ ty) = f t <> go ty
  go t@(EffectFunction ty) = f t <> go ty
  go other = f other

isTemplate :: Type -> Bool
isTemplate Template{} = True
isTemplate _ = False

argtype :: Maybe Type -> Maybe Type
argtype (Just (Function a _)) | everythingOnTypes (||) (== AutoType) a = Just AutoType
argtype (Just (Function a _)) = Just a
argtype (Just AutoType) = Just AutoType
argtype _ = Nothing

rettype :: Maybe Type -> Maybe Type
rettype (Just (Function _ b)) | everythingOnTypes (||) (== AutoType) b = Just AutoType
rettype (Just (Function _ b)) = Just b
rettype (Just (EffectFunction b)) = Just b
rettype (Just AutoType) = Just AutoType
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
templateVars = nub . sortBy (compare `on` runType) . everythingOnTypes (++) go
  where
  go :: Type -> [Type]
  go (Template t _) = [Template t []]
  go _ = []

templparams :: Type -> [TemplateInfo]
templparams = everythingOnTypes (++) go
  where
  go :: Type -> [TemplateInfo]
  go (Template p ts) = [(runType (Template p []), length ts)]
  go _ = []

templparams' :: Maybe Type -> [TemplateInfo]
templparams' = sortBy (compare `on` fst) . nub . maybe [] templparams

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
runQualifier CppExtern = "extern"
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
    go args (Template t [a], Native "eff_fn" []) =
      args ++ [(Template t [a], EffectFunction a)]
    go args (Template t [a], Array a') =
      args ++ (Template t [], Array anytype) : (go [] (a, a'))
    go args (Template t ts, Native t' ts') | length ts == length ts' =
      args ++ (Template t [], Native t' []) : concatMap (go []) (zip ts ts')
    go args (Template t ts, Native t' []) =
      args ++ [(Template t ts, Native t' ts)]
    go args (a@DeclType{}, a') = args ++ [(a, a')]
    go args (Function a b, Function a' b') = args ++ (go [] (a, a')) ++ (go [] (b, b'))
    go args (Array t, Array t') = go args (t, t')
    go args (Map ms, Map ms') = args ++ concatMap (go []) (zip (map snd ms) (map snd ms'))
    go args (EffectFunction b, EffectFunction b') = go args (b, b')
    go args (Native _ ts@(_:_), Native _ ts'@(_:_)) = args ++ concatMap (go []) (zip ts ts')
    go args (Native _ _, Native _ _) = args
    go args (Native _ ts, Map ts') | length ts == length ts' = args ++ concatMap (go []) (zip ts (map snd ts'))
    go args ((Primitive t), (Primitive t')) | t == t' = args
    go args (AutoType, _) = args -- TODO: is it ok to silence all of these?
    go args (t1', t2') = trace ("Mismatched type structure! " ++ show t1' ++ " ; " ++ show t2') args

onlyChanges :: Eq a => [(a, a)] -> [(a, a)]
onlyChanges = filter $ \(a,b) -> a /= b

templateFromKind :: (String, Maybe Kind) -> TemplateInfo
templateFromKind (name, Just f@(FunKind _ _)) = (makeUnique name, numFunKindArgs f)
  where
  numFunKindArgs :: Kind -> Int
  numFunKindArgs = everythingOnKinds (+) go
    where
    go :: Kind -> Int
    go (FunKind _ _) = 1
    go _ = 0
templateFromKind (name, _) = (makeUnique name, 0)
templateFromKind k = error $ show "Unsupported kind! (" ++ show k ++ ")"

addTemplateDefaults :: [TemplateInfo] -> [TemplateInfo]
addTemplateDefaults = map addDefault
  where
  addDefault ::  TemplateInfo -> TemplateInfo
  addDefault (name, 0) = (name ++ " = void", 0)
  addDefault (name, n) = (name ++ " = void" ++ show n, n)

remTemplateDefaults :: [TemplateInfo] -> [TemplateInfo]
remTemplateDefaults = map remDefault
  where
  remDefault ::  TemplateInfo -> TemplateInfo
  remDefault (name, 0) = (takeWhile (\c -> isAlphaNum c || c == '_') name, 0)
  remDefault (name, n) = (takeWhile (\c -> isAlphaNum c || c == '_') name, n)

-- TODO: do this more properly
anytype :: Type
anytype = Template [] []

makeUnique :: String -> String
makeUnique [] = []
makeUnique s@('_' : _) = s
makeUnique s = '_' : s

-- |
-- Used for rank-N types
--
forallsToAuto :: T.Type -> T.Type
forallsToAuto = T.everywhereOnTypes go
  where
  go :: T.Type -> T.Type
  go (T.ForAll _ _ _) = autoType
  go t = t

typevals :: [(Type, Type)] -> Qualified Ident
typevals ts = Qualified (Just $ ModuleName [ProperName C.prim])
                        (Ident $ intercalate ", " $ map (typeval . runType . snd) ts)
  where
  typeval :: String -> String
  typeval s = "typeval<" ++ s ++ ">"

tyFromExpr :: Expr Ann -> Maybe T.Type
tyFromExpr expr = go expr
  where
  go (Abs (_, _, t, _) _ _) = t
  go (App (_, _, t, _) _ _) = t
  go (Var (_, _, t, _) _) = t
  go (Literal (_, _, t, _) _) = t
  go (Accessor (_, _, t, _) _ _) = t
  go _ = Nothing
