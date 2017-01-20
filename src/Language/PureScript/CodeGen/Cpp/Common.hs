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
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.CodeGen.Cpp.Common where

import Prelude.Compat hiding (all, any, concatMap, last, init, null)

import Data.Char
import Data.Text hiding (foldl1, map)
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Language.PureScript.Constants as C
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeStringEither)

-- |
-- Convert an Ident into a valid C++11 identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved C++11 identifiers are suffixed with a '$'
--
--  * Symbols are prefixed with "$0" plus their ordinal value.
--
identToCpp :: Ident -> Text
identToCpp (Ident name) = safeName name
identToCpp (GenIdent _ _) = internalError "GenIdent in identToCpp"

safeName :: Text -> Text
safeName name
  | nameIsCppReserved name = name <> "$"
safeName name
  | Just ('$', s) <- uncons name
  , all isDigit s = name
safeName name
  | C.__superclass_ `isPrefixOf` name
  , any (=='.') name
  = "$" <> (escaped $ dotsTo '_' name)
safeName name
  | Just ('_', rest) <- uncons name
  , not (null rest)
  , isUpper (T.head rest) = "_$" <> escaped rest
safeName name = escaped name

escaped :: Text -> Text
escaped = escapeDoubleUnderscores . concatMap identCharToText

dotsTo :: Char -> Text -> Text
dotsTo chr' = T.map (\c -> if c == '.' then chr' else c)

codePoints :: PSString -> String
codePoints = map (either (chr . fromIntegral) id) . decodeStringEither

safeSymbol :: PSString -> Text
safeSymbol = T.concat . map identCharToText . codePoints

-- |
-- C++ actually reserves all identifiers containing double
-- underscores.
--
escapeDoubleUnderscores :: Text -> Text
escapeDoubleUnderscores = replace "__" "_$_"

unescapeDoubleUnderscores :: Text -> Text
unescapeDoubleUnderscores = replace "_$_" "__"

-- |
-- Test if a string is a valid Cpp identifier without escaping.
--
identNeedsEscaping :: Text -> Bool
identNeedsEscaping s = s /= safeName s

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToText :: Char -> Text
identCharToText c | isAlphaNum c = singleton c
identCharToText '_' = "_"
identCharToText '.' = "$dot"
identCharToText '$' = "$dollar"
identCharToText '~' = "$tilde"
identCharToText '=' = "$eq"
identCharToText '<' = "$less"
identCharToText '>' = "$greater"
identCharToText '!' = "$bang"
identCharToText '#' = "$hash"
identCharToText '%' = "$percent"
identCharToText '^' = "$up"
identCharToText '&' = "$amp"
identCharToText '|' = "$bar"
identCharToText '*' = "$times"
identCharToText '/' = "$div"
identCharToText '+' = "$plus"
identCharToText '-' = "$minus"
identCharToText ':' = "$colon"
identCharToText '\\' = "$bslash"
identCharToText '?' = "$qmark"
identCharToText '@' = "$at"
identCharToText '\'' = "$prime"
identCharToText c = "$0" <> (pack . show $ ord c)

-- |
-- Checks whether an identifier name is reserved in C++11.
--
nameIsCppReserved :: Text -> Bool
nameIsCppReserved name =
  name `elem` [ "alignas"
              , "alignof"
              , "and"
              , "and_eq"
              , "any"
              , "asm"
              , "assert"
              , "atomic_cancel"
              , "atomic_commit"
              , "atomic_noexcept"
              , "auto"
              , "bitand"
              , "bitor"
              , "bool"
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
              , "concept"
              , "cstring"
              , "ctor"
              , "decltype"
              , "default"
              , "define_symbol"
              , "delete"
              , "do"
              , "double"
              , "dynamic_cast"
              , "else"
              , "enum"
              , "errno"
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
              , "long"
              , "make_managed"
              , "make_managed_and_finalized"
              , "managed"
              , "math_errhandling"
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
              , "private"
              , "protected"
              , "public"
              , "register"
              , "reinterpret_cast"
              , "requires"
              , "return"
              , "runtime_error"
              , "setjmp"
              , "short"
              , "signed"
              , "sizeof"
              , "static"
              , "static_assert"
              , "static_cast"
              , "string"
              , "struct"
              , "switch"
              , "symbol"
              , "synchronized"
              , "template"
              , "the_value"
              , "this"
              , "thread_local"
              , "throw"
              , "true"
              , "try"
              , "typedef"
              , "typeid"
              , "typename"
              , "typeof"
              , "undefined"
              , "union"
              , "unknown_size"
              , "unsigned"
              , "using"
              , "va_end"
              , "virtual"
              , "void"
              , "volatile"
              , "wchar_t"
              , "while"
              , "xor"
              , "xor_eq" ] || properNameIsCppReserved name

normalizedName :: Text -> Text
normalizedName = unescapeDoubleUnderscores . normalizedName'
  where
  normalizedName' name
    | null name = ""
  normalizedName' name
    | last name == '$' = init name
  normalizedName' name
    | Just ('$', s) <- uncons name = s
  normalizedName' s = s

moduleNameToCpp :: ModuleName -> Text
moduleNameToCpp (ModuleName pns) =
  if properNameIsCppReserved name then name <> "$" else name
  where
  pnames = runProperName <$> pns
  name
    | foldl1 (||) (any (=='_') <$> pnames) = error "Module names can now contain underscores"
    | otherwise = intercalate "_" pnames

-- |
-- Checks whether a proper name is reserved in C++11.
--
properNameIsCppReserved :: Text -> Bool
properNameIsCppReserved name =
  name `elem` [ "Private"
              , "PureScript"
              ]
