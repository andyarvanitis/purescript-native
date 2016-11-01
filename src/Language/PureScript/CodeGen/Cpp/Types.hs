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

import Prelude.Compat

import Data.List

import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C

-- import Debug.Trace

data CppType = CppPrimitive String | CppAuto [CppTypeQual] | CppAny [CppTypeQual]
  deriving (Show, Read, Eq)

data CppTypeQual = CppConst | CppRef
  deriving (Show, Read, Eq)

-- |
-- Value C++11 qualifiers
--
data CppValueQual
  -- |
  -- C++ static qualifier
  --
  = CppStatic
  -- |
  -- Inline function
  --
  | CppInline
  -- |
  -- C++11+ constant expression "constexpr"
  --
  | CppConstExpr
  -- |
  -- Extern value
  --
  | CppExtern
  -- |
  -- Function or lambda is involved in recursion
  --
  | CppRecursive
  -- |
  -- Function or lambda is involved in recursion
  --
  | CppTopLevel
  deriving (Show, Read, Eq)

-- |
-- C++ lambda capture list
--
data CppCaptureType = CppCaptureAll
  deriving (Show, Read, Eq)

-- |
-- C++ object/map literal type
--
data CppObjectType = CppInstance | CppRecord
  deriving (Show, Eq)

runType :: CppType -> String
runType (CppPrimitive t) = t
runType (CppAuto []) = "auto"
runType (CppAny []) = "any"
runType typ =
  case typ of
    CppAuto qs -> rendered CppAuto qs
    CppAny qs -> rendered CppAny qs
    _ -> rendered CppAny []
  where
  rendered t qs
    | CppConst `elem` qs = "const " ++ (runType . t $ delete CppConst qs)
    | CppRef   `elem` qs = runType (t $ delete CppRef qs) ++ "&"
    | otherwise = runType (t qs)

runValueQual :: CppValueQual -> String
runValueQual CppStatic    = "static"
runValueQual CppInline    = "inline"
runValueQual CppConstExpr = "constexpr"
runValueQual CppExtern    = "extern"
runValueQual CppRecursive = ""
runValueQual CppTopLevel  = ""

runCaptureType :: CppCaptureType -> String
runCaptureType CppCaptureAll = "="

-- TODO: move or remove this
--
qualifiedToStr :: ModuleName -> (a -> Ident) -> Qualified a -> String
qualifiedToStr _ f (Qualified (Just (ModuleName [ProperName mn])) a) | mn == C.prim = runIdent $ f a
qualifiedToStr m f (Qualified (Just m') a) | m /= m' = moduleNameToCpp m' ++ "::" ++ identToCpp (f a)
qualifiedToStr _ f (Qualified _ a) = identToCpp (f a)

boolType :: CppType
boolType = CppPrimitive "bool"

intType :: CppType
intType = CppPrimitive "int"

doubleType :: CppType
doubleType = CppPrimitive "double"

stringType :: CppType
stringType = CppPrimitive "string"

charType :: CppType
charType = CppPrimitive "char"

voidType :: CppType
voidType = CppPrimitive "void"

mapType :: Int -> CppType
mapType = maptype
  where
  maptype 0 = mapprim "unknown_size"
  maptype n = mapprim $ show n
  mapprim s = CppPrimitive $ "any::map<" ++ s ++ ">"

dataType :: Int -> CppType
dataType n = CppPrimitive $ "any::data<" ++ show n ++ ">"

arrayType :: CppType
arrayType = CppPrimitive "any::array"

thunkMarkerType :: CppType
thunkMarkerType = CppPrimitive "any::as_thunk"

ctorKey :: String
ctorKey = "constructor"

constAnyRef :: Maybe CppType
constAnyRef = Just $ CppAny [CppConst, CppRef]

symbolname :: String -> String
symbolname = identToCpp . Ident
