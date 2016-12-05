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
import Text.Printf (printf)

import Language.PureScript.CodeGen.Cpp.AST

---------------------------------------------------------------------------------------------------
unicodeToUCNs :: Cpp -> Cpp
---------------------------------------------------------------------------------------------------
unicodeToUCNs (CppVar s) =
  CppVar (toUcns s)
unicodeToUCNs (CppSymbol s) =
  CppSymbol (toUcns s)
unicodeToUCNs (CppFunction s ss rty qs cpp) =
  CppFunction (toUcns s) ((\(a,t) -> (toUcns a, t)) <$> ss) rty qs cpp
unicodeToUCNs (CppLambda cps ss rty cpp) =
  CppLambda cps ((\(a,t) -> (toUcns a, t)) <$> ss) rty cpp
unicodeToUCNs (CppVariableIntroduction (ss,t) qs cpp) =
  CppVariableIntroduction (toUcns ss, t) qs cpp
unicodeToUCNs (CppEnum s t ss) =
  CppEnum (toUcns <$> s) t (toUcns <$> ss)
unicodeToUCNs (CppStruct s cpps) =
  CppStruct (toUcns s) cpps
unicodeToUCNs (CppNamespace s cpps) =
  CppNamespace (toUcns s) cpps
unicodeToUCNs (CppUseNamespace s) =
  CppUseNamespace (toUcns s)
unicodeToUCNs (CppTypeAlias (s1,ss) t s2) =
  CppTypeAlias (toUcns s1, (\(a,n) -> (toUcns a, n)) <$> ss) t (toUcns s2)
unicodeToUCNs cpp = cpp

---------------------------------------------------------------------------------------------------
toUcns :: String -> String
---------------------------------------------------------------------------------------------------
toUcns s | any (not . isAscii) s = concatMap toUcn s
  where
  toUcn :: Char -> String
  toUcn c | isAscii c = [c]
  toUcn c = printf "\\U%08x" $ ord c
toUcns s = s
