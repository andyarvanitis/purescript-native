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
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.CodeGen.Cpp.Types where

import Prelude.Compat

import Data.List
import Data.Monoid ((<>))
import Data.Text

data CppType = Primitive Text | Auto [TypeQual] | Any [TypeQual]
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

runType :: CppType -> Text
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
    | Const `elem` qs = "const " <> (runType . t $ delete Const qs)
    | Ref   `elem` qs = runType (t $ delete Ref qs) <> "&"
    | otherwise = runType (t qs)

runValueQual :: ValueQual -> Text
runValueQual Static    = "static"
runValueQual Inline    = "inline"
runValueQual ConstExpr = "constexpr"
runValueQual Extern    = "extern"
runValueQual Recursive = ""
runValueQual TopLevel  = ""

runCaptureType :: CaptureType -> Text
runCaptureType CaptureAll = "="

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

dictType :: Int -> CppType
dictType = typ
  where
  typ 0 = prim "unknown_size"
  typ n = prim . pack $ show n
  prim s = Primitive $ "any::dict<" <> s <> ">"

dataType :: Int -> CppType
dataType n = Primitive $ "any::data<" <> pack (show n) <> ">"

arrayType :: CppType
arrayType = Primitive "any::array"

recordType :: CppType
recordType = Primitive "any::record"

thunkMarkerType :: CppType
thunkMarkerType = Primitive "any::as_thunk"

dictNS :: Text
dictNS = "dict"

dataNS :: Text
dataNS = "data"

ctorNS :: Text
ctorNS = "ctor"

getCtor :: Text
getCtor = dataNS <> "::ctor"

constAnyRef :: Maybe CppType
constAnyRef = Just $ Any [Const, Ref]

-- Prevents any optimization changes to type
--
alwaysType :: CppType -> Maybe CppType
alwaysType t = Just . Primitive $ runType t
