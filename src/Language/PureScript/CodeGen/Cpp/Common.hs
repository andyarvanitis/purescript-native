-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Common
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common code generation utility functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp.Common where

import Data.Char
import Data.List (intercalate)

import Language.PureScript.Names

-- |
-- Convert an Ident into a valid C++11 identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved C++11 identifiers are wrapped with '_'
--
--  * Symbols are wrapped with '_' between a symbol name or their ordinal value.
--
identToCpp :: Ident -> String
identToCpp (Ident name) | nameIsCppReserved name = '_' : name ++ "_"
identToCpp (Ident name@('$' : s)) | all isDigit s = name
identToCpp (Ident name) = concatMap identCharToString name
identToCpp (Op op) = concatMap identCharToString op

-- |
-- Test if a string is a valid Cpp identifier without escaping.
--
identNeedsEscaping :: String -> Bool
identNeedsEscaping s = s /= identToCpp (Ident s)

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_' = "_"
identCharToString '.' = "_dot_"
identCharToString '$' = "_dollar_"
identCharToString '~' = "_tilde_"
identCharToString '=' = "_eq_"
identCharToString '<' = "_less_"
identCharToString '>' = "_greater_"
identCharToString '!' = "_bang_"
identCharToString '#' = "_hash_"
identCharToString '%' = "_percent_"
identCharToString '^' = "_up_"
identCharToString '&' = "_amp_"
identCharToString '|' = "_bar_"
identCharToString '*' = "_times_"
identCharToString '/' = "_div_"
identCharToString '+' = "_plus_"
identCharToString '-' = "_minus_"
identCharToString ':' = "_colon_"
identCharToString '\\' = "_bslash_"
identCharToString '?' = "_qmark_"
identCharToString '@' = "_at_"
identCharToString '\'' = "_prime_"
identCharToString c = '_' : show (ord c) ++ "_"

-- |
-- Checks whether an identifier name is reserved in C++11.
--
nameIsCppReserved :: String -> Bool
nameIsCppReserved name =
  name `elem` [ "alignas"
              , "alignof"
              , "and"
              , "and_eq"
              , "any"
              , "asm"
              , "assert"
              , "auto"
              , "bitand"
              , "bitor"
              , "bool"
              , "boost"
              , "break"
              , "case"
              , "cast"
              , "catch"
              , "char"
              , "char16_t"
              , "char32_t"
              , "class"
              , "compl"
              , "concept"
              , "const"
              , "constexpr"
              , "const_cast"
              , "continue"
              , "constructor"
              , "decltype"
              , "default"
              , "delete"
              , "do"
              , "double"
              , "dynamic_cast"
              , "else"
              , "enum"
              , "explicit"
              , "export"
              , "extern"
              , "false"
              , "final"
              , "float"
              , "for"
              , "friend"
              , "goto"
              , "if"
              , "import"
              , "inline"
              , "int"
              , "list"
              , "long"
              , "mutable"
              , "namespace"
              , "new"
              , "nil"
              , "noexcept"
              , "not"
              , "not_eq"
              , "NULL"
              , "null"
              , "nullptr"
              , "nullptr_t"
              , "operator"
              , "or"
              , "or_eq"
              , "override"
              , "param"
              , "private"
              , "protected"
              , "public"
              , "PureScript"
              , "register"
              , "reinterpret_cast"
              , "requires"
              , "return"
              , "runtime_error"
              , "shared_list"
              , "short"
              , "signed"
              , "sizeof"
              , "static"
              , "static_assert"
              , "static_cast"
              , "std"
              , "string"
              , "struct"
              , "switch"
              , "template"
              , "this"
              , "thread_local"
              , "throw"
              , "true"
              , "try"
              , "typedef"
              , "typeid"
              , "typename"
              , "typeof"
              , "union"
              , "unsigned"
              , "using"
              , "virtual"
              , "void"
              , "volatile"
              , "wchar_t"
              , "while"
              , "xor"
              , "xor_eq" ] || properNameIsCppReserved name

normalizedName :: String -> String
normalizedName ('_' : s) | last s == '_', s' <- init s, nameIsCppReserved s' = s'
normalizedName s = s

moduleNameToCpp :: ModuleName -> String
moduleNameToCpp (ModuleName pns) =
  let name = intercalate "_" (runProperName `map` pns)
  in if properNameIsCppReserved name then '_' : name ++ "_" else name

-- |
-- Checks whether a proper name is reserved in C++11.
--
properNameIsCppReserved :: String -> Bool
properNameIsCppReserved name =
  name `elem` [ "Private"
              , "PureScript" ]
