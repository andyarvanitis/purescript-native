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
--  * Symbols are given a "__" suffix following their name or ordinal value.
--
identToJs :: Ident -> String
identToJs (Ident name) | nameIsJsReserved name = name ++ "_"
identToJs (Ident name) | (x:_) <- name, not (isLetter x) = lowercaseStart $ concatMap identCharToString name
identToJs (Ident name) = concatMap identCharToString name
identToJs (Op op) = lowercaseStart $ concatMap identCharToString op

lowercaseStart :: String -> String
lowercaseStart (x:xs) = (toLower x : xs)

-- |
-- Test if a string is a valid JS identifier without escaping.
--
identNeedsEscaping :: String -> Bool
identNeedsEscaping s = s /= identToJs (Ident s)

-- |"_
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_' = "_"
identCharToString '.' = "Dot"
identCharToString '$' = "Dollar"
identCharToString '~' = "Tilde"
identCharToString '=' = "Eq"
identCharToString '<' = "Less"
identCharToString '>' = "Greater"
identCharToString '!' = "Bang"
identCharToString '#' = "Hash"
identCharToString '%' = "Percent"
identCharToString '^' = "Up"
identCharToString '&' = "Amp"
identCharToString '|' = "Bar"
identCharToString '*' = "Times"
identCharToString '/' = "Div"
identCharToString '+' = "Plus"
identCharToString '-' = "Minus"
identCharToString ':' = "Colon"
identCharToString '\\' = "Backslash"
identCharToString '?' = "Qmark"
identCharToString '@' = "At"
identCharToString '\'' = "Prime"
identCharToString c = show (ord c) ++ "__"

-- |
-- Checks whether an identifier name is reserved in Javascript.
--
nameIsJsReserved :: String -> Bool
nameIsJsReserved name =
  name `elem` [ "abstract"
              , "append"
              , "apply"
              , "bool"
              , "break"
              , "byte"
              , "cap"
              , "case"
              , "chan"
              , "char"
              , "close"
              , "complex"
              , "complex64"
              , "complex128"
              , "const"
              , "continue"
              , "copy"
              , "default"
              , "defer"
              , "delete"
              , "double"
              , "else"
              , "error"
              , "fallthrough"
              , "float"
              , "float32"
              , "float64"
              , "for"
              , "func"
              , "go"
              , "goto"
              , "if"
              , "imag"
              , "import"
              , "int"
              , "int8"
              , "int16"
              , "int32"
              , "int64"
              , "interface"
              , "iota"
              , "len"
              , "long"
              , "make"
              , "map"
              , "new"
              , "nil"
              , "package"
              , "panic"
              , "print"
              , "range"
              , "real"
              , "recover"
              , "rune"
              , "return"
              , "select"
              , "short"
              , "string"
              , "struct"
              , "switch"
              , "type"
              , "uint"
              , "uint8"
              , "uint16"
              , "uint32"
              , "uint64"
              , "uintptr"
              , "var"
              , "yield" ]

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) = intercalate "_" (runProperName `map` pns)

moduleNameToJs' :: ModuleName -> String
moduleNameToJs' (ModuleName pns) = intercalate "." (runProperName `map` pns)
