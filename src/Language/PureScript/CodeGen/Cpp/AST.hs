-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.AST
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Data types for the intermediate simplified-C++11 AST
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.CodeGen.Cpp.AST where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative, (<$>), (<*>))
import Data.Traversable (traverse)
#endif
import Control.Monad.Identity
import Data.Data
import Language.PureScript.Comments
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Traversals

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
-- Built-in binary operators
--
data BinaryOp
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division
  --
  | Divide
  -- |
  -- Remainder
  --
  | Modulus
  -- |
  -- TODO: revisit this
  --
  | Dot
  -- |
  -- Generic equality test
  --
  | Equal
  -- |
  -- Generic inequality test
  --
  | NotEqual
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqual
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqual
  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  --
  deriving (Show, Eq, Data, Typeable)

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
  -- A character literal
  --
  | CppCharLiteral Char
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
  -- An general property accessor expression (optional type, property, expr)
  --
  | CppAccessor Cpp Cpp
  -- |
  -- A function introduction (name, arguments, return type, qualifiers, body)
  --
  | CppFunction String [(String, Maybe Type)] (Maybe Type) [CppQualifier] Cpp
  -- |
  -- A lambda introduction (arguments, return type, body)
  --
  | CppLambda [CppCaptureType] [(String, Maybe Type)] (Maybe Type) Cpp
  -- |
  -- Value type cast
  --
  | CppCast Type Cpp
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
  -- A C++ namespace
  --
  | CppNamespace String [Cpp]
  -- |
  -- A C++ #include
  --
  | CppInclude String String
  -- |
  -- A C++ using namespace declaration
  --
  | CppUseNamespace String
  -- |
  -- Type alias, e.g. "using T = U" (new name and template types, original type)
  --
  | CppTypeAlias (String,[(String, Int)]) Type String
  -- |
  -- A variable introduction and optional initialization
  --
  | CppVariableIntroduction (String, Maybe Type) [CppQualifier] (Maybe Cpp)
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
  -- A sequence of declarations or expressions
  --
  | CppSequence [Cpp]
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
-- Expand a Cpp sequence (non-recursively)
--
expandSeq :: Cpp -> [Cpp]
expandSeq (CppSequence cpps) = cpps
expandSeq cpp = [cpp]

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
  go (CppAccessor prop j) = f (CppAccessor (go prop) (go j))
  go (CppFunction name args rty qs j) = f (CppFunction name args rty qs (go j))
  go (CppLambda cps args rty j) = f (CppLambda cps args rty (go j))
  go (CppCast t cpp) = f (CppCast t (go cpp))
  go (CppApp j cpp) = f (CppApp (go j) (map go cpp))
  go (CppConditional j1 j2 j3) = f (CppConditional (go j1) (go j2) (go j3))
  go (CppBlock cpp) = f (CppBlock (map go cpp))
  go (CppNamespace name cpp) = f (CppNamespace name (map go cpp))
  go (CppVariableIntroduction name qs j) = f (CppVariableIntroduction name qs (fmap go j))
  go (CppAssignment j1 j2) = f (CppAssignment (go j1) (go j2))
  go (CppWhile j1 j2) = f (CppWhile (go j1) (go j2))
  go (CppFor name j1 j2 j3) = f (CppFor name (go j1) (go j2) (go j3))
  go (CppIfElse j1 j2 j3) = f (CppIfElse (go j1) (go j2) (fmap go j3))
  go (CppReturn cpp) = f (CppReturn (go cpp))
  go (CppThrow cpp) = f (CppThrow (go cpp))
  go (CppLabel name cpp) = f (CppLabel name (go cpp))
  go (CppSequence cpp) = f (CppSequence (map go cpp))
  go (CppComment com j) = f (CppComment com (go j))
  go other = f other

everywhereOnCppTopDown :: (Cpp -> Cpp) -> Cpp -> Cpp
everywhereOnCppTopDown f = runIdentity . everywhereOnCppTopDownM (Identity . f)

everywhereOnCppTopDownM :: (Applicative m, Monad m) => (Cpp -> m Cpp) -> Cpp -> m Cpp
everywhereOnCppTopDownM f = f >=> go
  where
  f' = f >=> go
  go (CppUnary op j) = CppUnary op <$> f' j
  go (CppBinary op j1 j2) = CppBinary op <$> f' j1 <*> f' j2
  go (CppArrayLiteral cpp) = CppArrayLiteral <$> traverse f' cpp
  go (CppIndexer j1 j2) = CppIndexer <$> f' j1 <*> f' j2
  go (CppObjectLiteral cpp) = CppObjectLiteral <$> traverse (sndM f') cpp
  go (CppAccessor prop j) = CppAccessor prop <$> f' j
  go (CppFunction name args rty qs j) = CppFunction name args rty qs <$> f' j
  go (CppLambda cps args rty j) = CppLambda cps args rty <$> f' j
  go (CppCast t cpp) = CppCast t <$> f' cpp
  go (CppApp j cpp) = CppApp <$> f' j <*> traverse f' cpp
  go (CppConditional j1 j2 j3) = CppConditional <$> f' j1 <*> f' j2 <*> f' j3
  go (CppBlock cpp) = CppBlock <$> traverse f' cpp
  go (CppNamespace name cpp) = CppNamespace name <$> traverse f' cpp
  go (CppSequence cpp) = CppSequence <$> traverse f' cpp
  go (CppVariableIntroduction name qs j) = CppVariableIntroduction name qs <$> traverse f' j
  go (CppAssignment j1 j2) = CppAssignment <$> f' j1 <*> f' j2
  go (CppWhile j1 j2) = CppWhile <$> f' j1 <*> f' j2
  go (CppFor name j1 j2 j3) = CppFor name <$> f' j1 <*> f' j2 <*> f' j3
  go (CppIfElse j1 j2 j3) = CppIfElse <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (CppReturn j) = CppReturn <$> f' j
  go (CppThrow j) = CppThrow <$> f' j
  go (CppLabel name j) = CppLabel name <$> f' j
  go (CppComment com j) = CppComment com <$> f' j
  go other = f other

everythingOnCpp :: (r -> r -> r) -> (Cpp -> r) -> Cpp -> r
everythingOnCpp (<>) f = go
  where
  go j@(CppUnary _ j1) = f j <> go j1
  go j@(CppBinary _ j1 j2) = f j <> go j1 <> go j2
  go j@(CppArrayLiteral cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppIndexer j1 j2) = f j <> go j1 <> go j2
  go j@(CppObjectLiteral cpp) = foldl (<>) (f j) (map (go . snd) cpp)
  go j@(CppAccessor j1 j2) = f j <> go j1 <> go j2
  go j@(CppFunction _ _ _ _ j1) = f j <> go j1
  go j@(CppLambda _ _ _ j1) = f j <> go j1
  go j@(CppCast _ cpp) = f j <> go cpp
  go j@(CppApp j1 cpp) = foldl (<>) (f j <> go j1) (map go cpp)
  go j@(CppConditional j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(CppBlock cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppNamespace _ cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppVariableIntroduction _ _ (Just j1)) = f j <> go j1
  go j@(CppAssignment j1 j2) = f j <> go j1 <> go j2
  go j@(CppWhile j1 j2) = f j <> go j1 <> go j2
  go j@(CppFor _ j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(CppIfElse j1 j2 Nothing) = f j <> go j1 <> go j2
  go j@(CppIfElse j1 j2 (Just j3)) = f j <> go j1 <> go j2 <> go j3
  go j@(CppReturn j1) = f j <> go j1
  go j@(CppThrow j1) = f j <> go j1
  go j@(CppLabel _ j1) = f j <> go j1
  go j@(CppSequence cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppComment _ j1) = f j <> go j1
  go other = f other
