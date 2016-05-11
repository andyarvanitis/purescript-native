-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.Unused
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Removes unused variables
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Cpp.Optimizer.Unused where

import Prelude.Compat

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Optimizer.Common

import qualified Language.PureScript.Constants as C

removeCodeAfterReturnStatements :: Cpp -> Cpp
removeCodeAfterReturnStatements = everywhereOnCpp (removeFromBlock go)
  where
  go :: [Cpp] -> [Cpp]
  go cpps | not (any isCppReturn cpps) = cpps
          | otherwise = let (body, ret : _) = span (not . isCppReturn) cpps in body ++ [ret]
  isCppReturn (CppReturn _) = True
  isCppReturn _ = False

removeCodeAfterContinueStatements :: Cpp -> Cpp
removeCodeAfterContinueStatements = everywhereOnCpp (removeFromBlock go)
  where
  go :: [Cpp] -> [Cpp]
  go cpps | not (any isCppContinue cpps) = cpps
          | otherwise = let (body, ret : _) = span (not . isCppContinue) cpps in body ++ [ret]
  isCppContinue = (== CppContinue)

removeUnusedArg :: Cpp -> Cpp
removeUnusedArg = everywhereOnCpp convert
  where
  convert (CppFunction name [(arg,atyp)] rtyp qs body)
    | arg == C.__unused = CppFunction name [("",atyp)] rtyp qs body
  convert cpp = cpp
