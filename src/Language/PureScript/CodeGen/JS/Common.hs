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

module Language.PureScript.CodeGen.JS.Common where

import Data.Char
import Data.List (intercalate)

import Language.PureScript.Names

-- |
-- Convert an Ident into a valid Javascript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToJs :: Ident -> String
identToJs (Ident name) | nameIsJsReserved name = "__" ++ name
identToJs (Ident name) = concatMap identCharToString name
identToJs (Op op) = concatMap identCharToString op

-- |
-- Test if a string is a valid JS identifier without escaping.
--
identNeedsEscaping :: String -> Bool
identNeedsEscaping s = s /= identToJs (Ident s)

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_' = "_"
identCharToString '.' = "_dot"
identCharToString '$' = "_dollar"
identCharToString '~' = "_tilde"
identCharToString '=' = "_eq"
identCharToString '<' = "_less"
identCharToString '>' = "_greater"
identCharToString '!' = "_bang"
identCharToString '#' = "_hash"
identCharToString '%' = "_percent"
identCharToString '^' = "_up"
identCharToString '&' = "_amp"
identCharToString '|' = "_bar"
identCharToString '*' = "_times"
identCharToString '/' = "_div"
identCharToString '+' = "_plus"
identCharToString '-' = "_minus"
identCharToString ':' = "_colon"
identCharToString '\\' = "_bslash"
identCharToString '?' = "_qmark"
identCharToString '@' = "_at"
identCharToString '\'' = "_prime"
identCharToString c = '_' : show (ord c)

-- |
-- Checks whether an identifier name is reserved in Javascript.
--
nameIsJsReserved :: String -> Bool
nameIsJsReserved name =
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
              , "yield" ] || properNameIsJsReserved name

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) =
  let name = intercalate "_" (runProperName `map` pns)
  in if properNameIsJsReserved name then "__" ++ name else name

-- |
-- Checks whether a proper name is reserved in Javascript.
--
properNameIsJsReserved :: String -> Bool
properNameIsJsReserved name =
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
              , "JSON"
              , "Intl" ]
