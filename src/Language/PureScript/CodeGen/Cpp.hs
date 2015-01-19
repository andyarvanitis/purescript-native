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
module Language.PureScript.CodeGen.Cpp where

import Data.List (elemIndices, intercalate, nub, sort)
import Data.Char (isAlphaNum)

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

noOp :: JS
noOp = JSRaw []
-----------------------------------------------------------------------------------------------------------------------
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

typestr m a@(T.TypeApp _ _)
  | [t] <- dataCon m a = asDataTy t
  | (t:ts) <- dataCon m a = asDataTy $ t ++ '<' : intercalate "," ts ++ ">"
  | otherwise = "T"

typestr m (T.ForAll _ ty _) = typestr m ty
typestr _ (T.Skolem nm _ _) = '#' : nm
typestr _ (T.TypeVar nm) = '#' : nm
typestr m (T.TypeConstructor typ) = intercalate "::" . words $ brk tname
  where
    tname = qualifiedToStr m (Ident . runProperName) typ
    brk = map (\c -> if c=='.' then ' ' else c)

typestr _ t = "T"
-----------------------------------------------------------------------------------------------------------------------
fnArgStr :: ModuleName -> Maybe T.Type -> String
fnArgStr m (Just ((T.TypeApp
                    (T.TypeApp
                      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                       a) _)))
                         = typestr m a
fnArgStr _ _ = []
-----------------------------------------------------------------------------------------------------------------------
fnRetStr :: ModuleName -> Maybe T.Type -> String
fnRetStr m (Just ((T.TypeApp
                    (T.TypeApp
                      (T.TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function")))
                       _) b)))
                         = typestr m b
fnRetStr _ _ = []
-----------------------------------------------------------------------------------------------------------------------
dataCon :: ModuleName -> T.Type -> [String]
dataCon m (T.TypeApp a b) = (dataCon m a) ++ (dataCon m b)
dataCon m a = [typestr m a]
dataCon _ _ = []
-----------------------------------------------------------------------------------------------------------------------
fnName :: Maybe String -> String -> Maybe String
fnName Nothing name = Just name
fnName (Just t) name = Just (t ++ ' ' : (identToJs $ Ident name))
-----------------------------------------------------------------------------------------------------------------------
templTypes :: ModuleName -> Maybe T.Type -> String
templTypes m (Just t) =
  let s = typestr m t
      ss = (takeWhile isAlphaNum . flip drop s) <$> (map (+1) . elemIndices '#' $ s) in
      if null ss then "" else intercalate ", " (map ("typename " ++) . nub . sort $ ss) ++ "|"
templTypes _ _ = ""
-----------------------------------------------------------------------------------------------------------------------
stripImpls :: JS -> JS
stripImpls (JSNamespace name bs) = JSNamespace name (map stripImpls bs)
stripImpls (JSComment c e) = JSComment c (stripImpls e)
stripImpls (JSVariableIntroduction var (Just expr)) = JSVariableIntroduction var (Just $ stripImpls expr)
stripImpls (JSFunction fn args _) = JSFunction fn args noOp
stripImpls dat@(JSData _ _ _ _) = dat
stripImpls _ = noOp
-----------------------------------------------------------------------------------------------------------------------
stripDecls :: JS -> JS
stripDecls (JSVariableIntroduction var (Just expr)) = JSVariableIntroduction var (Just $ stripDecls expr)
stripDecls dat@(JSData _ _ _ _) = noOp
stripDecls js = js
-----------------------------------------------------------------------------------------------------------------------
dataTypes :: [Bind Ann] -> [JS]
dataTypes = map (JSVar . mkClass) . nub . filter (not . null) . map dataType
  where
    mkClass :: String -> String
    mkClass s = templateDecl ++ "struct " ++ rmType s ++ " { virtual ~" ++ rmType s ++ "(){} }"
      where
        templateDecl
          | t@('[':_:_:_) <- drop 1 $ getType s = "template" ++ '<' : intercalate ", " (("typename " ++) <$> read t) ++ "> "
          | otherwise = []
-----------------------------------------------------------------------------------------------------------------------
dataType :: Bind Ann -> String
dataType (NonRec _ (Constructor (_, _, _, _) name _ _)) = runProperName name
dataType _ = []
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
addType :: String -> String
addType t = '@' : t
-----------------------------------------------------------------------------------------------------------------------
getType :: String -> String
getType = dropWhile (/='@')
-----------------------------------------------------------------------------------------------------------------------
getSpecialization :: String -> String
getSpecialization s = case spec of
                        ('<':ss) -> take (length ss - 2) ss
                        _ -> []
  where
    spec = dropWhile (/='<') . drop 1 $ dropWhile (/='<') s
-----------------------------------------------------------------------------------------------------------------------
rmType :: String -> String
rmType = takeWhile (/='@')
