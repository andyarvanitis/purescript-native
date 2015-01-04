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
              , "yield" ]

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) = intercalate "_" (runProperName `map` pns)

moduleNameToJs' :: ModuleName -> String
moduleNameToJs' (ModuleName pns) = intercalate "." (runProperName `map` pns)
