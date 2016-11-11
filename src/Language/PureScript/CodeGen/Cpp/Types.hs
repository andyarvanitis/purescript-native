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

data CppType = Primitive String | Auto [TypeQual] | Any [TypeQual]
  deriving (Show, Read, Eq)

data TypeQual = Const | Ref
  deriving (Show, Read, Eq)

-- |
-- Value C++11 qualifiers
--
data ValueQual
  -- |
  -- C++ static qualifier
  --
  = Static
  -- |
  -- Inline function
  --
  | Inline
  -- |
  -- C++11+ constant expression "constexpr"
  --
  | ConstExpr
  -- |
  -- Extern value
  --
  | Extern
  -- |
  -- Function or lambda is involved in recursion
  --
  | Recursive
  -- |
  -- Function or lambda is involved in recursion
  --
  | TopLevel
  deriving (Show, Read, Eq)

-- |
-- C++ lambda capture list
--
data CaptureType = CaptureAll
  deriving (Show, Read, Eq)

-- |
-- C++ object/map literal type
--
data MapType = Instance | Record
  deriving (Show, Eq)

runType :: CppType -> String
runType (Primitive t) = t
runType (Auto []) = "auto"
runType (Any []) = "any"
runType typ =
  case typ of
    Auto qs -> rendered Auto qs
    Any qs -> rendered Any qs
    _ -> rendered Any []
  where
  rendered t qs
    | Const `elem` qs = "const " ++ (runType . t $ delete Const qs)
    | Ref   `elem` qs = runType (t $ delete Ref qs) ++ "&"
    | otherwise = runType (t qs)

runValueQual :: ValueQual -> String
runValueQual Static    = "static"
runValueQual Inline    = "inline"
runValueQual ConstExpr = "constexpr"
runValueQual Extern    = "extern"
runValueQual Recursive = ""
runValueQual TopLevel  = ""

runCaptureType :: CaptureType -> String
runCaptureType CaptureAll = "="

-- TODO: move or remove this
--
qualifiedToStr :: ModuleName -> (a -> Ident) -> Qualified a -> String
qualifiedToStr _ f (Qualified (Just (ModuleName [ProperName mn])) a)
  | mn == C.prim = runIdent $ f a
qualifiedToStr m f (Qualified (Just m') a)
  | m /= m' = moduleNameToCpp m' ++ "::" ++ identToCpp (f a)
qualifiedToStr _ f (Qualified _ a) = identToCpp (f a)

boolType :: CppType
boolType = Primitive "bool"

intType :: CppType
intType = Primitive "int"

doubleType :: CppType
doubleType = Primitive "double"

stringType :: CppType
stringType = Primitive "string"

charType :: CppType
charType = Primitive "char"

voidType :: CppType
voidType = Primitive "void"

mapType :: Int -> CppType
mapType = maptype
  where
  maptype 0 = mapprim "unknown_size"
  maptype n = mapprim $ show n
  mapprim s = Primitive $ "any::map<" ++ s ++ ">"

dataType :: Int -> CppType
dataType n = Primitive $ "any::data<" ++ show n ++ ">"

arrayType :: CppType
arrayType = Primitive "any::array"

thunkMarkerType :: CppType
thunkMarkerType = Primitive "any::as_thunk"

ctorKey :: String
ctorKey = "constructor"

constAnyRef :: Maybe CppType
constAnyRef = Just $ Any [Const, Ref]

symbolname :: String -> String
symbolname = identToCpp . Ident
