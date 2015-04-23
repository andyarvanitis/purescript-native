-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common code generation utility functions
--
-----------------------------------------------------------------------------

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
  name `elem` [ "abstract"
              , "any_map"
              , "arguments"
              , "auto"
              , "bool"
              , "break"
              , "byte"
              , "case"
              , "catch"
              , "char"
              , "class"
              , "const"
              , "continue"
              , "debugger"
              , "default"
              , "delete"
              , "do"
              , "double"
              , "else"
              , "enum"
              , "eval"
              , "export"
              , "extends"
              , "extern"
              , "final"
              , "finally"
              , "float"
              , "for"
              , "function"
              , "goto"
              , "if"
              , "implements"
              , "import"
              , "in"
              , "instanceof"
              , "int"
              , "interface"
              , "let"
              , "list"
              , "long"
              , "native"
              , "new"
              , "nil"
              , "null"
              , "NULL"
              , "nullptr"
              , "package"
              , "private"
              , "protected"
              , "public"
              , "return"
              , "short"
              , "static"
              , "super"
              , "switch"
              , "synchronized"
              , "this"
              , "throw"
              , "throws"
              , "transient"
              , "try"
              , "typeof"
              , "var"
              , "void"
              , "volatile"
              , "while"
              , "with"
              , "yield" ] || properNameIsCppReserved name

moduleNameToCpp :: ModuleName -> String
moduleNameToCpp (ModuleName pns) =
  let name = intercalate "_" (runProperName `map` pns)
  in if properNameIsCppReserved name then '_' : name ++ "_" else name

-- |
-- Checks whether a proper name is reserved in C++11.
--
properNameIsCppReserved :: String -> Bool
properNameIsCppReserved name =
  name `elem` [ "Infinity"
              , "NaN"
              , "Object"
              , "Function"
              , "Boolean"
              , "Error"
              , "EvalError"
              , "InternalError"
              , "RangeError"
              , "ReferenceError"
              , "SyntaxError"
              , "TypeError"
              , "URIError"
              , "Number"
              , "Math"
              , "Date"
              , "String"
              , "RegExp"
              , "Array"
              , "Int8Array"
              , "Uint8Array"
              , "Uint8ClampedArray"
              , "Int16Array"
              , "Uint16Array"
              , "Int32Array"
              , "Uint32Array"
              , "Float32Array"
              , "Float64Array"
              , "ArrayBuffer"
              , "DataView"
              , "CppON"
              , "Intl" ]
