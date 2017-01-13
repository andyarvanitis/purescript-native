-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Unicode
-- Copyright   :  (c) 2016 Andy Arvanitis
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Unicode support functions
--
-----------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp.Unicode
  ( unicodeToUCNs
  ) where

import Prelude.Compat
import Data.Char (isAscii, ord)
import Data.Text (Text, pack, unpack)
import Text.Printf (printf)

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.PSString (PSString, mkString)

---------------------------------------------------------------------------------------------------
unicodeToUCNs :: Cpp -> Cpp
---------------------------------------------------------------------------------------------------
unicodeToUCNs (CppVar s) =
  CppVar (toUCNs s)
unicodeToUCNs (CppSymbol s) =
  CppSymbol (toUCNs' s)
unicodeToUCNs (CppFunction s ss rty qs cpp) =
  CppFunction (toUCNs s) ((\(a,t) -> (toUCNs a, t)) <$> ss) rty qs cpp
unicodeToUCNs (CppLambda cps ss rty cpp) =
  CppLambda cps ((\(a,t) -> (toUCNs a, t)) <$> ss) rty cpp
unicodeToUCNs (CppVariableIntroduction (ss,t) qs cpp) =
  CppVariableIntroduction (toUCNs ss, t) qs cpp
unicodeToUCNs (CppEnum s t ss) =
  CppEnum (toUCNs <$> s) t (toUCNs <$> ss)
unicodeToUCNs (CppStruct s cpps) =
  CppStruct (toUCNs s) cpps
unicodeToUCNs (CppNamespace s cpps) =
  CppNamespace (toUCNs s) cpps
unicodeToUCNs (CppUseNamespace s) =
  CppUseNamespace (toUCNs s)
unicodeToUCNs (CppTypeAlias (s1,ss) t s2) =
  CppTypeAlias (toUCNs s1, (\(a,n) -> (toUCNs a, n)) <$> ss) t (toUCNs s2)
unicodeToUCNs cpp = cpp

---------------------------------------------------------------------------------------------------
toUCNs :: Text -> Text
---------------------------------------------------------------------------------------------------
toUCNs = pack . concatMap toUCN . unpack

---------------------------------------------------------------------------------------------------
toUCNs' :: PSString -> PSString
---------------------------------------------------------------------------------------------------
toUCNs' = mkString . pack . concatMap toUCN . codePoints

---------------------------------------------------------------------------------------------------
toUCN :: Char -> String
---------------------------------------------------------------------------------------------------
toUCN c | isAscii c = [c]
toUCN c = printf "\\U%08x" $ ord c
