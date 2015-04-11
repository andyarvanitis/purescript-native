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

module Language.PureScript.CodeGen.Cpp.Types where

import Data.List
import Data.Char
import Data.Maybe
import Data.Function (on)
import Control.Applicative
import Language.PureScript.Names
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.CodeGen.Cpp.AST
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Types as T

import Debug.Trace

data Type = Native String
          | Function Type Type
          | Data Type
          | Specialized Type [Type]
          | List Type
          | Template String
          | ParamTemplate String [Type]
          | EffectFunction Type
          deriving (Eq, Show)

runType :: Type -> String
runType (Native name) = name
runType tt@(Function a b) = typeName tt ++ '<' : runType a ++ "," ++ runType b ++ ">"
runType tt@(EffectFunction b) = typeName tt ++ '<' : runType b ++ ">"
runType tt@(Data t) = typeName tt ++ '<' : runType t ++ ">"
runType (Specialized t []) = runType t
runType (Specialized t ts) = runType t ++ '<' : (intercalate "," $ map runType ts) ++ ">"
runType tt@(List t) = typeName tt ++ '<' : runType t ++ ">"
runType (Template []) = error "Bad template parameter"
runType tt@(Template name) =  typeName tt ++ capitalize name
  where
  capitalize :: String -> String
  capitalize (c:cs) = toUpper c : cs
  capitalize s = s
runType (ParamTemplate name ts) = runType (Template name) ++ '<' : (intercalate "," $ map runType ts) ++ ">"

typeName :: Type -> String
typeName Function{} = "fn"
typeName EffectFunction{} = "eff_fn"
typeName Data{} = "data"
typeName List{} = "list"
typeName Template{} = ""
typeName _ = ""

mktype :: ModuleName -> T.Type -> Maybe Type

mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Number")))    = Just $ Native "double"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "String")))    = Just $ Native "string"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Boolean")))   = Just $ Native "bool"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Integral")))  = Just $ Native "int"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Int")))       = Just $ Native "int"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Integer")))   = Just $ Native "long long"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Char")))      = Just $ Native "char"

mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prelude"])) (ProperName "Float")))  = Just $ Native "double"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prelude"])) (ProperName "Double"))) = Just $ Native "double"

mktype _ (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               T.REmpty) _) = error "Need to supprt func() T"

mktype m (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               a) b) | Just a' <- mktype m a, Just b' <- mktype m b = Just $ Function a' b'
                     | otherwise = Nothing

-- This covers ((->) r)
mktype m (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
             T.TypeVar{}) = Just $ Native (typeName (Function (Template []) (Template [])))

mktype m (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Array")))
             a) | Just t <- mktype m a = Just $ List t
                | otherwise = Nothing

mktype _ (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Object")))
             T.REmpty) = Just $ Native "std::nullptr_t"

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

mktype m (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName ([ProperName "Control",
                                                              ProperName "Monad",
                                                              ProperName "Eff"]))) (ProperName "Eff")))
            _) = Just $ Native (typeName (EffectFunction (Template [])))

mktype m app@T.TypeApp{}
  | (name, tys@(_:_)) <- tyapp app [] = Just $ ParamTemplate (identToCpp $ Ident name) tys
  where
    tyapp :: T.Type -> [Type] -> (String, [Type])
    tyapp (T.TypeApp (T.TypeVar name) b) ts | Just b' <- mktype m b = (identToCpp $ Ident name, b':ts)
    tyapp (T.TypeApp (T.Skolem name _ _) b) ts | Just b' <- mktype m b = (identToCpp $ Ident name, b':ts)
    tyapp (T.TypeApp inner@(T.TypeApp _ _) t) ts | Just t' <- mktype m t = tyapp inner (t':ts)
    tyapp _ _ = ([],[])

mktype m (T.TypeApp T.Skolem{} b) = mktype m b

mktype m app@(T.TypeApp a b)
  | (T.TypeConstructor _) <- a, [t] <- dataCon m app = Just $ Data t
  | (T.TypeConstructor _) <- a, (t:ts) <- dataCon m app = Just $ Data (Specialized t ts)
  | (T.TypeConstructor _) <- b, [t] <- dataCon m app = Just $ Data t
  | (T.TypeConstructor _) <- b, (t:ts) <- dataCon m app = Just $ Data (Specialized t ts)

mktype m (T.ForAll _ ty _) = mktype m ty
mktype _ (T.Skolem name _ _) = Just $ Template (identToCpp $ Ident name)
mktype _ (T.TypeVar name) = Just $ Template (identToCpp $ Ident name)
mktype _ (T.TUnknown n) = Just $ Template ('T' : show n)
mktype m (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function"))) =
  Just $ Native (typeName (Function (Template []) (Template [])))
mktype m (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Array"))) =
  Just $ Native (typeName (List (Template [])))
mktype m (T.TypeConstructor (Qualified (Just (ModuleName ([ProperName "Control",
                                                           ProperName "Monad",
                                                           ProperName "Eff"]))) (ProperName "Eff"))) =
  Just $ Native (typeName (EffectFunction (Template [])))
mktype m a@(T.TypeConstructor _) = Just $ Data (Native $ qualDataTypeName m a)
mktype m (T.ConstrainedType _ ty) = mktype m ty
mktype _ (T.RCons _ _ _) = Just $ Template "rowType"
mktype _ T.REmpty = Nothing
mktype _ b = error $ "Unknown type: " ++ show b

typestr :: ModuleName -> T.Type -> String
typestr m t | Just t' <- mktype m t = runType t'
            | otherwise = []

arity :: Maybe Type -> Maybe Int
arity (Just (Function _ b)) = Just (1 + fromMaybe 0 (arity (Just b)))
arity _ = Nothing

argtype :: Maybe Type -> Maybe Type
argtype (Just (Function a _)) = Just a
argtype _ = Nothing

argtype' :: ModuleName -> T.Type -> String
argtype' m = maybe [] runType . argtype . mktype m

rettype :: Maybe Type -> Maybe Type
rettype (Just (Function _ b)) = Just b
rettype (Just (EffectFunction b)) = Just b
rettype _ = Nothing

rettype' :: ModuleName -> T.Type -> String
rettype' m = maybe [] runType . rettype . mktype m

fnTypesN :: Int -> Type -> [Type]
fnTypesN 0 t = [t]
fnTypesN n (Function a b) = a : types (n - 1) b
  where
  types :: Int -> Type -> [Type]
  types 0 t = [t]
  types n' (Function a' b') = a' : types (n' - 1) b'
  types _ _ = []
fnTypesN _ _ = []

templparams :: Type -> [(String, Int)]
templparams t@(Template a) = [(runType t, 0)]
templparams (ParamTemplate p ts) = concatMap templparams ts ++ [(runType (Template p), length ts)]
templparams (Function a b) = templparams a ++ templparams b
templparams (EffectFunction b) = templparams b
templparams (List a) = templparams a
templparams (Data a) = templparams a
templparams _ = []

templparams' :: Maybe Type -> [(String, Int)]
templparams' = sortBy (compare `on` fst) . nub . maybe [] templparams

dataCon :: ModuleName -> T.Type -> [Type]
dataCon m (T.TypeApp a b) = (dataCon m a) ++ (dataCon m b)
dataCon m a@(T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) _))
  | Just a' <- mktype m a = [a']
  | otherwise = []
dataCon m a@(T.TypeConstructor _) = [Native $ qualDataTypeName m a]
dataCon m a
  | Just a' <- mktype m a = [a']
  | otherwise = []

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

templateArgs :: (Type, Type) -> [(String,String)]
templateArgs = nubBy ((==) `on` fst) . sortBy (compare `on` fst). go []
  where
    go :: [(String,String)] -> (Type, Type) -> [(String,String)]
    go args (a@Template{}, a') = args ++ [(runType a, runType a')]
    go args (ParamTemplate p ts, ParamTemplate p' ts') =
      args ++ (runType (Template p), runType (Template p')) : concatMap (go []) (zip ts ts')
    go args (ParamTemplate t [a, b], Function a' b') =
      args ++ (runType (Template t), typeName (Function anytype anytype)) : (go [] (a, a')) ++ (go [] (b, b'))
    go args (ParamTemplate t [b], EffectFunction b') =
      args ++ (runType (Template t), typeName (EffectFunction anytype)) : (go [] (b, b'))
    go args (ParamTemplate t [a], List a') =
      args ++ (runType (Template t), typeName (List anytype)) : (go [] (a, a'))
--  go args (ParamTemplate t [a], Data a') =
--    args ++ (runType (Template t), typeName (Data a')) : (go [] (a, a'))
--  go args (Specialized t [], Specialized t' []) = go args (t, t')
--  go args (Specialized t ts, Specialized t' ts') = args ++ (go [] (t, t')) ++ (concatMap (go []) $ zip ts ts')
--  go args (a@(Template "rowType"), a'@(Template "rowType")) = args ++ [(runType a, [])]
    go args (Function a b, Function a' b') = args ++ (go [] (a, a')) ++ (go [] (b, b'))
    go args (EffectFunction b, EffectFunction b') = args ++ (go [] (b, b'))
    go args (Data t, Data t') = go args (t, t')
    go args (List t, List t') = args ++ go [] (t, t')
    go args (Native t, Native t')
      | t == t' = args
      | otherwise = error ("Type conflict! " ++ t ++ " ; " ++ t')
    go _ (t1', t2') = error ("Mismatched type structure! " ++ show t1' ++ " ; " ++ show t2')

    anytype :: Type
    anytype = Template []