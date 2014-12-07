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

module Language.PureScript.CodeGen.Common where

import Data.Char
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Function (on)

import qualified Data.Map as M

import Language.PureScript.Names
import Language.PureScript.Environment
import Language.PureScript.Types

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

-- |"_
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
              , "yield" ]

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) = intercalate "_" (runProperName `map` pns)

-- |
-- Finds the value stored for a data constructor in the current environment.
-- This is a partial function, but if an invalid type has reached this far then
-- something has gone wrong in typechecking.
--
lookupConstructor :: Environment -> Qualified ProperName -> (DataDeclType, ProperName, Type)
lookupConstructor e ctor = fromMaybe (error "Data constructor not found") $ ctor `M.lookup` dataConstructors e

-- |
-- Checks whether a data constructor is the only constructor for that type, used
-- to simplify the check when generating code for binders.
--
isOnlyConstructor :: Environment -> Qualified ProperName -> Bool
isOnlyConstructor e ctor = numConstructors (ctor, lookupConstructor e ctor) == 1
  where
  numConstructors :: (Qualified ProperName, (DataDeclType, ProperName, Type)) -> Int
  numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors e
  typeConstructor :: (Qualified ProperName, (DataDeclType, ProperName, Type)) -> (ModuleName, ProperName)
  typeConstructor (Qualified (Just moduleName) _, (_, tyCtor, _)) = (moduleName, tyCtor)
  typeConstructor _ = error "Invalid argument to isOnlyConstructor"

-- |
-- Checks whether a data constructor is for a newtype.
--
isNewtypeConstructor :: Environment -> Qualified ProperName -> Bool
isNewtypeConstructor e ctor = case lookupConstructor e ctor of
  (Newtype, _, _) -> True
  (Data, _, _) -> False

-- |
-- Checks the number of arguments a data constructor accepts.
--
getConstructorArity :: Environment -> Qualified ProperName -> Int
getConstructorArity e = go . (\(_, _, ctors) -> ctors) . lookupConstructor e
  where
  go :: Type -> Int
  go (TypeApp (TypeApp f _) t) | f == tyFunction = go t + 1
  go (ForAll _ ty _) = go ty
  go _ = 0

-- |
-- Checks whether a data constructor has no arguments, for example, `Nothing`.
--
isNullaryConstructor :: Environment -> Qualified ProperName -> Bool
isNullaryConstructor e = (== 0) . getConstructorArity e
