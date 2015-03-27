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
--  * Reserved C++11 identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToCpp :: Ident -> String
identToCpp (Ident name) | nameIsCppReserved name = "$$" ++ name
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
identCharToString '.' = "$dot"
identCharToString '$' = "$dollar"
identCharToString '~' = "$tilde"
identCharToString '=' = "$eq"
identCharToString '<' = "$less"
identCharToString '>' = "$greater"
identCharToString '!' = "$bang"
identCharToString '#' = "$hash"
identCharToString '%' = "$percent"
identCharToString '^' = "$up"
identCharToString '&' = "$amp"
identCharToString '|' = "$bar"
identCharToString '*' = "$times"
identCharToString '/' = "$div"
identCharToString '+' = "$plus"
identCharToString '-' = "$minus"
identCharToString ':' = "$colon"
identCharToString '\\' = "$bslash"
identCharToString '?' = "$qmark"
identCharToString '@' = "$at"
identCharToString '\'' = "$prime"
identCharToString c = '$' : show (ord c)

-- |
-- Checks whether an identifier name is reserved in C++11.
--
nameIsCppReserved :: String -> Bool
nameIsCppReserved name =
  name `elem` [ "abstract"
              , "arguments"
              , "boolean"
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
              , "long"
              , "native"
              , "new"
              , "null"
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
  in if properNameIsCppReserved name then "$$" ++ name else name

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
