-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.AST
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Data types for the intermediate simplified-C++11 AST
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.CodeGen.Cpp.AST where

import Data.Data

import Language.PureScript.CoreImp.Operators
import Language.PureScript.Comments

-- |
-- Built-in unary operators
--
data CppUnaryOp
  -- |
  -- Numeric negation
  --
  = CppNegate
  -- |
  -- Boolean negation
  --
  | CppNot
  -- |
  -- Bitwise negation
  --
  | CppBitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | CppPositive
  -- |
  -- Constructor
  --
  | CppNew deriving (Show, Eq, Data, Typeable)

-- |
-- Data type for simplified C++11 expressions
--
data Cpp
  -- |
  -- A numeric literal
  --
  = CppNumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | CppStringLiteral String
  -- |
  -- A boolean literal
  --
  | CppBooleanLiteral Bool
  -- |
  -- A unary operator application
  --
  | CppUnary CppUnaryOp Cpp
  -- |
  -- A binary operator application
  --
  | CppBinary BinaryOp Cpp Cpp
  -- |
  -- An array literal
  --
  | CppArrayLiteral [Cpp]
  -- |
  -- An array indexer expression
  --
  | CppIndexer Cpp Cpp
  -- |
  -- An object literal
  --
  | CppObjectLiteral [(String, Cpp)]
  -- |
  -- An object property accessor expression
  --
  | CppAccessor String Cpp
  -- |
  -- A function introduction (optional name, arguments, body)
  --
  | CppFunction (Maybe String) [String] Cpp
  -- |
  -- Function application
  --
  | CppApp Cpp [Cpp]
  -- |
  -- Variable
  --
  | CppVar String
  -- |
  -- Conditional expression
  --
  | CppConditional Cpp Cpp Cpp
  -- |
  -- A block of expressions in braces
  --
  | CppBlock [Cpp]
  -- |
  -- A variable introduction and optional initialization
  --
  | CppVariableIntroduction String (Maybe Cpp)
  -- |
  -- A variable assignment
  --
  | CppAssignment Cpp Cpp
  -- |
  -- While loop
  --
  | CppWhile Cpp Cpp
  -- |
  -- For loop
  --
  | CppFor String Cpp Cpp Cpp
  -- |
  -- ForIn loop
  --
  | CppForIn String Cpp Cpp
  -- |
  -- If-then-else statement
  --
  | CppIfElse Cpp Cpp (Maybe Cpp)
  -- |
  -- Return statement
  --
  | CppReturn Cpp
  -- |
  -- Throw statement
  --
  | CppThrow Cpp
  -- |
  -- Type-Of operator
  --
  | CppTypeOf Cpp
  -- |
  -- InstanceOf test
  --
  | CppInstanceOf Cpp Cpp
  -- |
  -- Labelled statement
  --
  | CppLabel String Cpp
  -- |
  -- Break statement
  --
  | CppBreak String
  -- |
  -- Continue statement
  --
  | CppContinue String
  -- |
  -- Empty statement/expression
  --
  | CppNoOp
  -- |
  -- Marker for header/source split
  --
  | CppEndOfHeader
  -- |
  -- Raw C++11 (generated when parsing fails for an inline foreign import declaration)
  --
  | CppRaw String
  -- |
  -- Commented C++11
  --
  | CppComment [Comment] Cpp deriving (Show, Eq, Data, Typeable)

--
-- Traversals
--

everywhereOnCpp :: (Cpp -> Cpp) -> Cpp -> Cpp
everywhereOnCpp f = go
  where
  go :: Cpp -> Cpp
  go (CppUnary op j) = f (CppUnary op (go j))
  go (CppBinary op j1 j2) = f (CppBinary op (go j1) (go j2))
  go (CppArrayLiteral cpp) = f (CppArrayLiteral (map go cpp))
  go (CppIndexer j1 j2) = f (CppIndexer (go j1) (go j2))
  go (CppObjectLiteral cpp) = f (CppObjectLiteral (map (fmap go) cpp))
  go (CppAccessor prop j) = f (CppAccessor prop (go j))
  go (CppFunction name args j) = f (CppFunction name args (go j))
  go (CppApp j cpp) = f (CppApp (go j) (map go cpp))
  go (CppConditional j1 j2 j3) = f (CppConditional (go j1) (go j2) (go j3))
  go (CppBlock cpp) = f (CppBlock (map go cpp))
  go (CppVariableIntroduction name j) = f (CppVariableIntroduction name (fmap go j))
  go (CppAssignment j1 j2) = f (CppAssignment (go j1) (go j2))
  go (CppWhile j1 j2) = f (CppWhile (go j1) (go j2))
  go (CppFor name j1 j2 j3) = f (CppFor name (go j1) (go j2) (go j3))
  go (CppForIn name j1 j2) = f (CppForIn name (go j1) (go j2))
  go (CppIfElse j1 j2 j3) = f (CppIfElse (go j1) (go j2) (fmap go j3))
  go (CppReturn cpp) = f (CppReturn (go cpp))
  go (CppThrow cpp) = f (CppThrow (go cpp))
  go (CppTypeOf cpp) = f (CppTypeOf (go cpp))
  go (CppLabel name cpp) = f (CppLabel name (go cpp))
  go (CppInstanceOf j1 j2) = f (CppInstanceOf (go j1) (go j2))
  go (CppComment com j) = f (CppComment com (go j))
  go other = f other

everywhereOnCppTopDown :: (Cpp -> Cpp) -> Cpp -> Cpp
everywhereOnCppTopDown f = go . f
  where
  go :: Cpp -> Cpp
  go (CppUnary op j) = CppUnary op (go (f j))
  go (CppBinary op j1 j2) = CppBinary op (go (f j1)) (go (f j2))
  go (CppArrayLiteral cpp) = CppArrayLiteral (map (go . f) cpp)
  go (CppIndexer j1 j2) = CppIndexer (go (f j1)) (go (f j2))
  go (CppObjectLiteral cpp) = CppObjectLiteral (map (fmap (go . f)) cpp)
  go (CppAccessor prop j) = CppAccessor prop (go (f j))
  go (CppFunction name args j) = CppFunction name args (go (f j))
  go (CppApp j cpp) = CppApp (go (f j)) (map (go . f) cpp)
  go (CppConditional j1 j2 j3) = CppConditional (go (f j1)) (go (f j2)) (go (f j3))
  go (CppBlock cpp) = CppBlock (map (go . f) cpp)
  go (CppVariableIntroduction name j) = CppVariableIntroduction name (fmap (go . f) j)
  go (CppAssignment j1 j2) = CppAssignment (go (f j1)) (go (f j2))
  go (CppWhile j1 j2) = CppWhile (go (f j1)) (go (f j2))
  go (CppFor name j1 j2 j3) = CppFor name (go (f j1)) (go (f j2)) (go (f j3))
  go (CppForIn name j1 j2) = CppForIn name (go (f j1)) (go (f j2))
  go (CppIfElse j1 j2 j3) = CppIfElse (go (f j1)) (go (f j2)) (fmap (go . f) j3)
  go (CppReturn j) = CppReturn (go (f j))
  go (CppThrow j) = CppThrow (go (f j))
  go (CppTypeOf j) = CppTypeOf (go (f j))
  go (CppLabel name j) = CppLabel name (go (f j))
  go (CppInstanceOf j1 j2) = CppInstanceOf (go (f j1)) (go (f j2))
  go (CppComment com j) = CppComment com (go (f j))
  go other = f other

everythingOnCpp :: (r -> r -> r) -> (Cpp -> r) -> Cpp -> r
everythingOnCpp (<>) f = go
  where
  go j@(CppUnary _ j1) = f j <> go j1
  go j@(CppBinary _ j1 j2) = f j <> go j1 <> go j2
  go j@(CppArrayLiteral cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppIndexer j1 j2) = f j <> go j1 <> go j2
  go j@(CppObjectLiteral cpp) = foldl (<>) (f j) (map (go . snd) cpp)
  go j@(CppAccessor _ j1) = f j <> go j1
  go j@(CppFunction _ _ j1) = f j <> go j1
  go j@(CppApp j1 cpp) = foldl (<>) (f j <> go j1) (map go cpp)
  go j@(CppConditional j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(CppBlock cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppVariableIntroduction _ (Just j1)) = f j <> go j1
  go j@(CppAssignment j1 j2) = f j <> go j1 <> go j2
  go j@(CppWhile j1 j2) = f j <> go j1 <> go j2
  go j@(CppFor _ j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(CppForIn _ j1 j2) = f j <> go j1 <> go j2
  go j@(CppIfElse j1 j2 Nothing) = f j <> go j1 <> go j2
  go j@(CppIfElse j1 j2 (Just j3)) = f j <> go j1 <> go j2 <> go j3
  go j@(CppReturn j1) = f j <> go j1
  go j@(CppThrow j1) = f j <> go j1
  go j@(CppTypeOf j1) = f j <> go j1
  go j@(CppLabel _ j1) = f j <> go j1
  go j@(CppInstanceOf j1 j2) = f j <> go j1 <> go j2
  go j@(CppComment _ j1) = f j <> go j1
  go other = f other
