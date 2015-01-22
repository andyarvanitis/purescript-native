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
identToJs (Ident name) | nameIsJsReserved name = '_' : name
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
              , "alignas"
              , "alignof"
              , "and"
              , "and_eq"
              , "asm"
              , "auto"
              , "bitand"
              , "bitor"
              , "bool"
              , "break"
              , "byte"
              , "case"
              , "cast"
              , "catch"
              , "char"
              , "char16_t"
              , "char32_t"
              , "class"
              , "compl"
              , "const"
              , "constexpr"
              , "const_cast"
              , "continue"
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
              , "finally"
              , "float"
              , "fn"
              , "for"
              , "friend"
              , "goto"
              , "if"
              , "import"
              , "in"
              , "inline"
              , "instanceof"
              , "int"
              , "list"
              , "long"
              , "make_data"
              , "mutable"
              , "namespace"
              , "new"
              , "noexcept"
              , "not"
              , "not_eq"
              , "nullptr"
              , "operator"
              , "or"
              , "or_eq"
              , "private"
              , "protected"
              , "public"
              , "register"
              , "reinterpret_cast"
              , "return"
              , "short"
              , "signed"
              , "sizeof"
              , "static"
              , "static_assert"
              , "static_cast"
              , "std"
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
              , "union"
              , "unsigned"
              , "using"
              , "virtual"
              , "void"
              , "volatile"
              , "wchar_t"
              , "while"
              , "xor"
              , "xor_eq"
              , "yield" ]

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) = intercalate "_" (runProperName `map` pns)
