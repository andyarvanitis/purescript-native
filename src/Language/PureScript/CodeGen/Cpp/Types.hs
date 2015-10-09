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
import Data.Data

import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C

-- import Debug.Trace

data CppType = CppPrimitive String | CppAuto | CppAny [CppTypeQual]
  deriving (Eq, Show, Data, Typeable)

data CppTypeQual = CppConst | CppRef
  deriving (Eq, Show, Data)

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

  deriving (Show, Eq, Data, Typeable)

-- |
-- C++ lambda capture list
--
data CppCaptureType = CppCaptureAll
  deriving (Show, Eq, Data, Typeable)

runType :: CppType -> String
runType (CppPrimitive t) = t
runType CppAuto = "auto"
runType (CppAny qs)
  | CppConst `elem` qs = "const " ++ (runType . CppAny $ delete CppConst qs)
  | CppRef   `elem` qs = runType (CppAny $ delete CppRef qs) ++ "&"
  | otherwise = "any"

runValueQual :: CppValueQual -> String
runValueQual CppStatic    = "static"
runValueQual CppInline    = "inline"
runValueQual CppConstExpr = "constexpr"
runValueQual CppExtern    = "extern"
runValueQual CppRecursive = ""

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
intType = CppPrimitive "long"

doubleType :: CppType
doubleType = CppPrimitive "double"

stringType :: CppType
stringType = CppPrimitive "string"

charType :: CppType
charType = CppPrimitive "char"

mapType :: CppType
mapType = CppPrimitive "any::map"

arrayType :: CppType
arrayType = CppPrimitive "any::vector"

thunkMarkerType :: CppType
thunkMarkerType = CppPrimitive "any::as_thunk"

unthunkMarkerValue :: String
unthunkMarkerValue = "any::unthunk"

ctorKey :: String
ctorKey = "constructor"
