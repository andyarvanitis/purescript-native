-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp
-- Copyright   :  (c) _ 2013
-- License     :  MIT
--
-- Maintainer  :  _
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code in the simplified Javascript intermediate representation from Purescript code
--
-----------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp where

import Data.List (elemIndices, intercalate, nub, sort, sortBy)
import Data.Char (isAlphaNum, toUpper)
import Data.Function (on)

import Control.Applicative

import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreFn
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Types as T

import Debug.Trace

headerPreamble :: [JS]
headerPreamble =
  [ JSRaw "// Standard includes"
  , JSRaw "//"
  , JSRaw "#include <functional>"
  , JSRaw "#include <memory>"
  , JSRaw "#include <vector>"
  , JSRaw "#include <string>"
  , JSRaw "#include <iostream>"
  , JSRaw " "
  , JSRaw "// Type support"
  , JSRaw " "
  , JSRaw "template <typename T, typename Enable = void>"
  , JSRaw "struct ADT;"
  , JSRaw " "
  , JSRaw "template <typename T>"
  , JSRaw "struct ADT <T, typename std::enable_if<std::is_fundamental<T>::value>::type> {"
  , JSRaw "  using type = T;"
  , JSRaw "  template <typename... ArgTypes>"
  , JSRaw "  constexpr static auto make(ArgTypes... args) -> type {"
  , JSRaw "    return T(args...);"
  , JSRaw "  }"
  , JSRaw "};"
  , JSRaw " "
  , JSRaw "template <typename T>"
  , JSRaw "struct ADT <T, typename std::enable_if<!std::is_fundamental<T>::value>::type> {"
  , JSRaw "  using type = std::shared_ptr<T>;"
  , JSRaw "  template <typename... ArgTypes>"
  , JSRaw "  constexpr static auto make(ArgTypes... args) -> type {"
  , JSRaw "    return std::make_shared<T>(args...);"
  , JSRaw "  }"
  , JSRaw "};"
  , JSRaw " "
  , JSRaw "// Type aliases"
  , JSRaw "//"
  , JSRaw "template <typename T, typename U> using fn = std::function<U(T)>;"
  , JSRaw "template <typename T> using data = typename ADT<T>::type;"
  , JSRaw "template <typename T> using list = std::vector<T>;"
  , JSRaw "using string = std::string;"
  , JSRaw " "
  , JSRaw "// Function aliases"
  , JSRaw " "
  , JSRaw "template <typename T, typename... ArgTypes>"
  , JSRaw "constexpr auto make_data(ArgTypes... args) -> typename ADT<T>::type {"
  , JSRaw "  return ADT<T>::make(args...);"
  , JSRaw "}"
  , JSRaw " "
  , JSRaw "template <typename T, typename U>"
  , JSRaw "constexpr auto cast(const std::shared_ptr<U>& a) -> T {"
  , JSRaw "  return *(std::dynamic_pointer_cast<T>(a));"
  , JSRaw "}"
  , JSRaw " "
  , JSRaw "template <typename T, typename U>"
  , JSRaw "constexpr auto instanceof(const std::shared_ptr<U>& a) -> std::shared_ptr<T> {"
  , JSRaw "  return std::dynamic_pointer_cast<T>(a);"
  , JSRaw "}"
  , JSRaw " "
  ]

noOp :: JS
noOp = JSRaw []
-----------------------------------------------------------------------------------------------------------------------
data Type = Native String
          | Function Type Type
          | Data Type
          | Specialized Type [Type]
          | List Type
          | Template String
          deriving (Eq)

-----------------------------------------------------------------------------------------------------------------------
instance Show Type where
  show (Native name) = name
  show (Function a b) = "fn<" ++ show a ++ "," ++ show b ++ ">"
  show (Data t) = "data<" ++ show t ++ ">"
  show (Specialized t []) = show t
  show (Specialized t ts) = show t ++ '<' : (intercalate "," $ map show ts) ++ ">"
  show (List t) = "list<" ++ show t ++ ">"
  show (Template (c:cs)) = '#' : toUpper c : cs
  show (Template []) = error "Bad template parameter"
  -- show (Empty) = []

-----------------------------------------------------------------------------------------------------------------------
mktype :: ModuleName -> T.Type -> Maybe Type

mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Number")))  = Just $ Native "long"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "String")))  = Just $ Native "string"
mktype _ (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Boolean"))) = Just $ Native "bool"

mktype _ (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               T.REmpty) _) = error "Need to supprt func() T"

mktype m (T.TypeApp
            (T.TypeApp
              (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
               a) b) | Just a' <- mktype m a, Just b' <- mktype m b = Just $ Function a' b'
                     | otherwise = Nothing

mktype m (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Array")))
             a) | Just t <- mktype m a = Just $ List t
                | otherwise = Nothing

mktype _ (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Object")))
             T.REmpty) = Just $ Native "std::nullptr_t"

mktype m (T.TypeApp
            (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Object")))
             a) = Just $ Native ("struct{" ++ typestr m a ++ "}")

mktype m (T.TypeApp T.TypeVar{} b) = mktype m b
mktype m (T.TypeApp T.Skolem{}  b) = mktype m b

mktype m app@(T.TypeApp a b)
  | (T.TypeConstructor _) <- a, [t] <- dataCon m app = Just $ Data t
  | (T.TypeConstructor _) <- a, (t:ts) <- dataCon m app = Just $ Data (Specialized t ts)
  | (T.TypeConstructor _) <- b, [t] <- dataCon m app = Just $ Data t
  | (T.TypeConstructor _) <- b, (t:ts) <- dataCon m app = Just $ Data (Specialized t ts)

mktype m (T.TypeApp a b) | Just a' <- mktype m a, Just b' <- mktype m b = Just $ Function a' b'
                         | otherwise = Nothing

mktype m (T.ForAll _ ty _) = mktype m ty
mktype _ (T.Skolem name _ _) = Just $ Template name
mktype _ (T.TypeVar name) = Just $ Template name
mktype m a@(T.TypeConstructor _) = Just $ Data (Native $ qualDataTypeName m a)
mktype m (T.ConstrainedType _ ty) = mktype m ty
mktype _ T.REmpty = Nothing
mktype _ b = error $ "Unknown type: " ++ show b

typestr :: ModuleName -> T.Type -> String
typestr m t | Just t' <- mktype m t = show t'
            | otherwise = []

-----------------------------------------------------------------------------------------------------------------------
fnArgStr :: ModuleName -> Maybe T.Type -> String
fnArgStr m (Just ((T.TypeApp
                    (T.TypeApp
                      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                       a) _)))
                         = typestr m a
fnArgStr m (Just (T.ForAll _ ty _)) = fnArgStr m (Just ty)
fnArgStr _ _ = []
-----------------------------------------------------------------------------------------------------------------------
fnRetStr :: ModuleName -> Maybe T.Type -> String
fnRetStr m (Just ((T.TypeApp
                    (T.TypeApp
                      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                       _) b)))
                         = typestr m b
fnRetStr m (Just (T.ForAll _ ty _)) = fnRetStr m (Just ty)
fnRetStr _ _ = []
-----------------------------------------------------------------------------------------------------------------------
dataCon :: ModuleName -> T.Type -> [Type]
dataCon m (T.TypeApp a b) = (dataCon m a) ++ (dataCon m b)
dataCon m a@(T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) _))
  | Just a' <- mktype m a = [a']
  | otherwise = []
dataCon m a@(T.TypeConstructor _) = [Native $ qualDataTypeName m a]
dataCon m a
  | Just a' <- mktype m a = [a']
  | otherwise = []
-----------------------------------------------------------------------------------------------------------------------
qualDataTypeName :: ModuleName -> T.Type -> String
qualDataTypeName m (T.TypeConstructor typ) = intercalate "::" . words $ brk tname
  where
    tname = qualifiedToStr m (Ident . runProperName) typ
    brk = map (\c -> if c=='.' then ' ' else c)
qualDataTypeName _ _ = []
-----------------------------------------------------------------------------------------------------------------------
fnName :: Maybe String -> String -> Maybe String
fnName Nothing name = Just name
fnName (Just t) name = Just (t ++ ' ' : (identToJs $ Ident name))
-----------------------------------------------------------------------------------------------------------------------
templTypes :: String -> String
templTypes s
  | ('#' `elem` s) = intercalate ", " (("typename "++) <$> templParms s) ++ "|"
templTypes _ = []
-----------------------------------------------------------------------------------------------------------------------
templTypes' :: ModuleName -> Maybe T.Type -> String
templTypes' m (Just t)
  | s <- typestr m t = templTypes s
templTypes' _ _ = ""
-----------------------------------------------------------------------------------------------------------------------
stripImpls :: JS -> JS
stripImpls (JSNamespace name bs) = JSNamespace name (map stripImpls bs)
stripImpls (JSSequence bs) = JSSequence (map stripImpls bs)
stripImpls (JSComment c e) = JSComment c (stripImpls e)
stripImpls (JSVariableIntroduction var (Just (JSFunction (Just name) [arg] ret@(JSBlock [JSReturn (JSApp _ [JSVar arg'])]))))
  | ((last $ words arg) == arg') = JSVariableIntroduction var (Just (JSFunction (Just $ name ++ " inline") [arg] ret))
stripImpls imp@(JSVariableIntroduction _ (Just (JSFunction (Just name) _ _))) | '|' `elem` name = imp
stripImpls (JSVariableIntroduction var (Just expr)) = JSVariableIntroduction var (Just $ stripImpls expr)
stripImpls (JSFunction fn args _) = JSFunction fn args noOp
stripImpls dat@(JSData _ _ _ _) = dat
stripImpls _ = noOp
-----------------------------------------------------------------------------------------------------------------------
stripDecls :: JS -> JS
stripDecls (JSNamespace name bs) = JSNamespace name (map stripDecls bs)
stripDecls (JSSequence bs) = JSSequence (map stripDecls bs)
stripDecls (JSComment c e) = JSComment c (stripDecls e)
stripDecls (JSVariableIntroduction _ (Just (JSFunction (Just _) [arg] (JSBlock [JSReturn (JSApp _ [JSVar arg'])]))))
  | ((last $ words arg) == arg') = noOp
stripDecls (JSVariableIntroduction _ (Just (JSFunction (Just name) _ _))) | '|' `elem` name = noOp
stripDecls (JSVariableIntroduction var (Just expr)) = JSVariableIntroduction var (Just $ stripDecls expr)
stripDecls (JSData _ _ _ _) = noOp
stripDecls js = js
-----------------------------------------------------------------------------------------------------------------------
dataTypes :: [Bind Ann] -> [JS]
dataTypes = map (JSVar . mkClass) . nub . filter (not . null) . map dataType
  where
    mkClass :: String -> String
    mkClass s = templateDecl ++ "struct " ++ rmType s ++ " { virtual ~" ++ rmType s ++ "(){} };"
      where
        templateDecl
          | t@('[':_:_:_) <- drop 1 $ getType s = "template " ++ '<' : intercalate ", " (("typename " ++) <$> read t) ++ "> "
          | otherwise = []
-----------------------------------------------------------------------------------------------------------------------
dataType :: Bind Ann -> String
dataType (NonRec _ (Constructor (_, _, _, Just IsNewtype) _ _ _)) = []
dataType (NonRec _ (Constructor (_, _, _, _) name _ _)) = runProperName name
dataType _ = []
-----------------------------------------------------------------------------------------------------------------------
getAppSpecType :: ModuleName -> Expr Ann -> Int -> String
getAppSpecType m e l
    | (App (_, _, Just dty, _) _ _) <- e,
      (_:ts) <- show $ dataCon m dty,
      ty@(_:_) <- drop l (show ts) = '<' : intercalate "," (map show ty) ++ ">"
    | otherwise = []
-----------------------------------------------------------------------------------------------------------------------
qualifiedToStr :: ModuleName -> (a -> Ident) -> Qualified a -> String
qualifiedToStr _ f (Qualified (Just (ModuleName [ProperName mn])) a) | mn == C.prim = runIdent $ f a
qualifiedToStr m f (Qualified (Just m') a) | m /= m' = moduleNameToJs m' ++ "::" ++ identToJs (f a)
qualifiedToStr _ f (Qualified _ a) = identToJs (f a)
-----------------------------------------------------------------------------------------------------------------------
asDataTy :: String -> String
asDataTy t = "data<" ++ t ++ ">"
-----------------------------------------------------------------------------------------------------------------------
mkData :: String -> String
mkData t = "make_data<" ++ t ++ ">"
-----------------------------------------------------------------------------------------------------------------------
dataCtorName :: String
dataCtorName = "ctor"
-----------------------------------------------------------------------------------------------------------------------
mkDataFn :: String -> String
mkDataFn t = t ++ ':':':':dataCtorName
-----------------------------------------------------------------------------------------------------------------------
mkUnique :: String -> String
mkUnique s = '_' : s ++ "_"

mkUnique' :: Ident -> Ident
mkUnique' (Ident s) = Ident $ mkUnique s
mkUnique' ident = ident
-----------------------------------------------------------------------------------------------------------------------
addType :: String -> String
addType t = '@' : t
-----------------------------------------------------------------------------------------------------------------------
getType :: String -> String
getType = dropWhile (/='@')
-----------------------------------------------------------------------------------------------------------------------
getSpecialization :: String -> String
getSpecialization s = case spec of
                        ('<':ss) -> '<' : take (length ss - 2) ss ++ ">"
                        _ -> []
  where
    spec = dropWhile (/='<') . drop 1 $ dropWhile (/='<') s
-----------------------------------------------------------------------------------------------------------------------
rmType :: String -> String
rmType = takeWhile (/='@')
-----------------------------------------------------------------------------------------------------------------------
fromAngles :: String -> Int -> String
fromAngles [] _ = []
fromAngles _ 0 = []
fromAngles (x@'<':xs) n = x : fromAngles xs (n+1)
fromAngles (x@'>':xs) n = x : fromAngles xs (n-1)
fromAngles (x:xs)     n = x : fromAngles xs n
-----------------------------------------------------------------------------------------------------------------------
afterAngles :: String -> Int -> String
afterAngles [] _ = []
afterAngles xs 0 = xs
afterAngles ('<':xs) n = afterAngles xs (n+1)
afterAngles ('>':xs) n = afterAngles xs (n-1)
afterAngles (_:xs)   n = afterAngles xs n
-----------------------------------------------------------------------------------------------------------------------
getArg :: String -> String
getArg ('f':'n':'<':xs) = "fn<" ++ fromAngles xs 1
getArg xs = takeWhile (/=',') xs
-----------------------------------------------------------------------------------------------------------------------
getRet :: String -> String
getRet ('f':'n':'<':xs) = drop 1 $ afterAngles xs 1
getRet xs = drop 1 $ dropWhile (/=',') xs
-----------------------------------------------------------------------------------------------------------------------
templParms :: String -> [String]
templParms s = nub . sort $ (takeWhile isAlphaNum . flip drop s) <$> (map (+1) . elemIndices '#' $ s)

extractTypes :: String -> [String]
extractTypes = words . extractTypes'

extractTypes' :: String -> String
extractTypes' [] = []
extractTypes' ('f':'n':'<':xs) = ' ' : extractTypes' xs
extractTypes' (x:xs) | not (isAlphaNum x) = ' ' : extractTypes' xs
extractTypes' (x:xs) = x : extractTypes' xs

-----------------------------------------------------------------------------------------------------------------------
-- fillTemplates :: Type -> Type -> Type
-- fillTemplates (Native t) (Native t') | t == t' = (Native t')
-- fillTemplates (Function a b) (Function a' b') = Function (fillTemplates a a') (fillTemplates b b')
-- fillTemplates (Data t) (Data t') = Data (fillTemplates t t')
-- fillTemplates (Specialized t []) (Specialized t' []) = Specialized (fillTemplates t t') []
-- fillTemplates (Specialized t ts) (Specialized t' ts') = Specialized (fillTemplates t t') (zipWith fillTemplates ts ts')
-- fillTemplates (List t) (List t') = List (fillTemplates t t')
-- fillTemplates (Template _) a' = a'
-- fillTemplates Empty Empty = Empty
-- fillTemplates _ _ = error "Mismatched type structure!"

templateSpec :: Maybe Type -> Maybe Type -> String
templateSpec (Just t1) (Just t2)
  | args@(_:_) <- filter (/='#') . intercalate "," $ snd <$> templateArgs t1 t2 = '<' : args ++ ">"
templateSpec _ _ = []

templateArgs :: Type -> Type -> [(String,String)]
templateArgs t1 t2 = nub. sortBy (compare `on` fst) $ templateArgs' [] t1 t2

templateArgs' :: [(String,String)] -> Type -> Type -> [(String,String)]
templateArgs' args (Native t) (Native t') | t == t' = args
templateArgs' args (Function a b) (Function a' b') = args ++ (templateArgs' [] a a') ++ (templateArgs' [] b b')
templateArgs' args (Data t) (Data t') = templateArgs' args t t'
templateArgs' args (Specialized t []) (Specialized t' []) = templateArgs' args t t'
templateArgs' args (Specialized t ts) (Specialized t' ts') = args ++ (templateArgs' [] t t') ++ (concat $ zipWith (templateArgs' []) ts ts')
templateArgs' args (List t) (List t') = templateArgs' args t t'
templateArgs' args (Template a) (Template a') = args ++ [(a, a')]
templateArgs' args (Template a) a' = args ++ [(a, show a')]
-- templateArgs' args Empty Empty = args
templateArgs' _ t1 t2 = error $ "Mismatched type structure! " ++ show t1 ++ " ; " ++ show t2
-----------------------------------------------------------------------------------------------------------------------
exprFnTy :: ModuleName -> Expr Ann -> Maybe Type
exprFnTy m (App (_, _, Just ty, _) val a)
  | Just nextTy <- exprFnTy m val = Just nextTy
  | Just a' <- declFnTy m a,
    Just b' <- mktype m ty = Just $ Function a' b'
  | Just b' <- mktype m ty = Just b'
exprFnTy _ _ = Nothing

-----------------------------------------------------------------------------------------------------------------------
declFnTy :: ModuleName -> Expr Ann -> Maybe Type
declFnTy m (Var (_, _, Just ty, _) _) = mktype m ty -- drop 3 . init $ typestr m ty -- strip outer "fn<>"
declFnTy m (App _ val _) = declFnTy m val
declFnTy _ _ = Nothing -- error $ "Can't find type: " ++ show m ++ ' ' : show t
