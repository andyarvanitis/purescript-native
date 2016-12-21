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

import Prelude.Compat

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative, (<$>), (<*>))
import Data.Traversable (traverse)
#endif
import Control.Monad.Identity
import Data.Text (Text)
import Language.PureScript.Comments
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Traversals

-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  -- |
  -- Get size
  --
  | Size
  -- |
  -- Check if empty
  --
  | Empty
  deriving (Show, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
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
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo
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
  deriving (Show, Eq)

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
  | CppStringLiteral Text
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
  | CppUnary UnaryOperator Cpp
  -- |
  -- A binary operator application
  --
  | CppBinary BinaryOperator Cpp Cpp
  -- |
  -- An array literal
  --
  | CppArrayLiteral [Cpp]
  -- |
  -- A data literal
  --
  | CppDataLiteral [Cpp]
  -- |
  -- An enum definition (optional name, optional type)
  --
  | CppEnum (Maybe Text) (Maybe CppType) [Text]
  -- |
  -- An array indexer expression
  --
  | CppIndexer Cpp Cpp
  -- |
  -- An object literal
  --
  | CppMapLiteral MapType [(Cpp, Cpp)]
  -- |
  -- An general property accessor expression (property, expr)
  --
  | CppAccessor Cpp Cpp
  -- |
  -- A function introduction (name, arguments, return type, qualifiers, body)
  --
  | CppFunction Text [(Text, Maybe CppType)] (Maybe CppType) [ValueQual] Cpp
  -- |
  -- A lambda introduction (arguments, return type, body)
  --
  | CppLambda [CaptureType] [(Text, Maybe CppType)] (Maybe CppType) Cpp
  -- |
  -- Value type cast
  --
  | CppCast CppType Cpp
  -- |
  -- Call to getter from templated map type
  --
  | CppMapGet Cpp Cpp
  -- |
  -- Call to getter from templated data type
  --
  | CppDataGet Cpp Cpp
  -- |
  -- Function application
  --
  | CppApp Cpp [Cpp]
  -- |
  -- Variable
  --
  | CppVar Text
  -- |
  -- Unique system-wide name/constant
  --
  | CppSymbol Text
  -- |
  -- Define unique system-wide name/constant
  --
  | CppDefineSymbol Text
  -- |
  -- A block of expressions in braces
  --
  | CppBlock [Cpp]
  -- |
  -- A C++ namespace
  --
  | CppNamespace Text [Cpp]
  -- |
  -- An C++ struct declaration (name, members)
  --
  | CppStruct Text [Cpp]
  -- |
  -- A C++ #include
  --
  | CppInclude Text Text
  -- |
  -- A C++ using namespace declaration
  --
  | CppUseNamespace Text
  -- |
  -- Type alias, e.g. "using T = U" (new name and template types, original type)
  --
  | CppTypeAlias (Text,[(Text, Int)]) CppType Text
  -- |
  -- A variable introduction and optional initialization
  --
  | CppVariableIntroduction (Text, Maybe CppType) [ValueQual] (Maybe Cpp)
  -- |
  -- A variable assignment
  --
  | CppAssignment Cpp Cpp
  -- |
  -- While loop
  --
  | CppWhile Cpp Cpp
  -- |
  -- If-then-else statement
  --
  | CppIfElse Cpp Cpp (Maybe Cpp)
  -- |
  -- Switch statement
  --
  | CppSwitch Cpp [(Cpp, Cpp)] (Maybe Cpp)
  -- |
  -- Return statement
  --
  | CppReturn Cpp
  -- |
  -- Throw statement
  --
  | CppThrow Cpp
  -- |
  -- Continue statement
  --
  | CppContinue
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
  | CppRaw Text
  -- |
  -- Commented C++11
  --
  | CppComment [Comment] Cpp
  deriving (Show, Eq)

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
  go (CppDataLiteral cpp) = f (CppDataLiteral (map go cpp))
  go (CppIndexer j1 j2) = f (CppIndexer (go j1) (go j2))
  go (CppMapLiteral t cpps) = f (CppMapLiteral t (map (fmap go) cpps))
  go (CppAccessor prop j) = f (CppAccessor (go prop) (go j))
  go (CppFunction name args rty qs j) = f (CppFunction name args rty qs (go j))
  go (CppLambda cps args rty j) = f (CppLambda cps args rty (go j))
  go (CppCast t cpp) = f (CppCast t (go cpp))
  go (CppMapGet j1 j2) = f (CppMapGet (go j1) (go j2))
  go (CppDataGet j1 j2) = f (CppDataGet (go j1) (go j2))
  go (CppApp j cpp) = f (CppApp (go j) (map go cpp))
  go (CppBlock cpp) = f (CppBlock (map go cpp))
  go (CppNamespace name cpp) = f (CppNamespace name (map go cpp))
  go (CppStruct name cpp) = f (CppStruct name (map go cpp))
  go (CppVariableIntroduction name qs j) = f (CppVariableIntroduction name qs (fmap go j))
  go (CppAssignment j1 j2) = f (CppAssignment (go j1) (go j2))
  go (CppWhile j1 j2) = f (CppWhile (go j1) (go j2))
  go (CppIfElse j1 j2 j3) = f (CppIfElse (go j1) (go j2) (fmap go j3))
  go (CppSwitch cpp cpps d) = f (CppSwitch (go cpp) (map (fmap go) cpps) (fmap go d))
  go (CppReturn cpp) = f (CppReturn (go cpp))
  go (CppThrow cpp) = f (CppThrow (go cpp))
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
  go (CppDataLiteral cpp) = CppDataLiteral <$> traverse f' cpp
  go (CppIndexer j1 j2) = CppIndexer <$> f' j1 <*> f' j2
  go (CppMapLiteral t cpps) = CppMapLiteral t <$> traverse (pairM f' f') cpps
  go (CppAccessor prop j) = CppAccessor prop <$> f' j
  go (CppFunction name args rty qs j) = CppFunction name args rty qs <$> f' j
  go (CppLambda cps args rty j) = CppLambda cps args rty <$> f' j
  go (CppCast t cpp) = CppCast t <$> f' cpp
  go (CppMapGet j1 j2) = CppMapGet <$> f' j1 <*> f' j2
  go (CppDataGet j1 j2) = CppDataGet <$> f' j1 <*> f' j2
  go (CppApp j cpp) = CppApp <$> f' j <*> traverse f' cpp
  go (CppBlock cpp) = CppBlock <$> traverse f' cpp
  go (CppNamespace name cpp) = CppNamespace name <$> traverse f' cpp
  go (CppStruct name cpp) = CppStruct name <$> traverse f' cpp
  go (CppVariableIntroduction name qs j) = CppVariableIntroduction name qs <$> traverse f' j
  go (CppAssignment j1 j2) = CppAssignment <$> f' j1 <*> f' j2
  go (CppWhile j1 j2) = CppWhile <$> f' j1 <*> f' j2
  go (CppIfElse j1 j2 j3) = CppIfElse <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (CppSwitch cpp cpps d) = CppSwitch <$> f' cpp <*> traverse (pairM f' f') cpps <*> traverse f' d
  go (CppReturn j) = CppReturn <$> f' j
  go (CppThrow j) = CppThrow <$> f' j
  go (CppComment com j) = CppComment com <$> f' j
  go other = f other

everythingOnCpp :: (r -> r -> r) -> (Cpp -> r) -> Cpp -> r
everythingOnCpp (<>) f = go
  where
  go j@(CppUnary _ j1) = f j <> go j1
  go j@(CppBinary _ j1 j2) = f j <> go j1 <> go j2
  go j@(CppArrayLiteral cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppDataLiteral cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppIndexer j1 j2) = f j <> go j1 <> go j2
  go j@(CppMapLiteral _ cpps) = foldl (<>) (f j) ((map (go . fst) cpps) ++ (map (go . snd) cpps))
  go j@(CppAccessor j1 j2) = f j <> go j1 <> go j2
  go j@(CppFunction _ _ _ _ j1) = f j <> go j1
  go j@(CppLambda _ _ _ j1) = f j <> go j1
  go j@(CppCast _ cpp) = f j <> go cpp
  go j@(CppMapGet j1 j2) = f j <> go j1 <> go j2
  go j@(CppDataGet j1 j2) = f j <> go j1 <> go j2
  go j@(CppApp j1 cpp) = foldl (<>) (f j <> go j1) (map go cpp)
  go j@(CppBlock cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppNamespace _ cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppStruct _ cpp) = foldl (<>) (f j) (map go cpp)
  go j@(CppVariableIntroduction _ _ (Just j1)) = f j <> go j1
  go j@(CppAssignment j1 j2) = f j <> go j1 <> go j2
  go j@(CppWhile j1 j2) = f j <> go j1 <> go j2
  go j@(CppIfElse j1 j2 Nothing) = f j <> go j1 <> go j2
  go j@(CppIfElse j1 j2 (Just j3)) = f j <> go j1 <> go j2 <> go j3
  go j@(CppSwitch cpp cpps Nothing) = foldl (<>) (f j <> go cpp) ((map (go . fst) cpps) ++ (map (go . snd) cpps))
  go j@(CppSwitch cpp cpps (Just d)) = foldl (<>) (f j <> go cpp) ((map (go . fst) cpps) ++ (map (go . snd) cpps)) <> go d
  go j@(CppReturn j1) = f j <> go j1
  go j@(CppThrow j1) = f j <> go j1
  go j@(CppComment _ j1) = f j <> go j1
  go other = f other
