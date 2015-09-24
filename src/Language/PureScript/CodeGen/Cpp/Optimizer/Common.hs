-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.Common
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common functions used by the various optimizer phases
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Cpp.Optimizer.Common where

import Data.Maybe (fromMaybe)

import Language.PureScript.CodeGen.Cpp.AST

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

replaceIdent :: String -> Cpp -> Cpp -> Cpp
replaceIdent var1 cpp = everywhereOnCpp replace
  where
  replace (CppVar var2) | var1 == var2 = cpp
  replace other = other

replaceIdents :: [(String, Cpp)] -> Cpp -> Cpp
replaceIdents vars = everywhereOnCpp replace
  where
  replace v@(CppVar var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: String -> Cpp -> Bool
isReassigned var1 = everythingOnCpp (||) check
  where
  check :: Cpp -> Bool
  check (CppFunction _ args _ _ _) | var1 `elem` (map fst args) = True
  check (CppVariableIntroduction arg _ _) | var1 == fst arg = True
  check (CppAssignment (CppVar arg) _) | var1 == arg = True
  check (CppFor arg _ _ _) | var1 == arg = True
  check _ = False

isRebound :: Cpp -> Cpp -> Bool
isRebound cpp d = any (\v -> isReassigned v d || isUpdated v d) (everythingOnCpp (++) variablesOf cpp)
  where
  variablesOf (CppVar var) = [var]
  variablesOf _ = []

isUsed :: String -> Cpp -> Bool
isUsed var1 = everythingOnCpp (||) check
  where
  check :: Cpp -> Bool
  check (CppVar var2) | var1 == var2 = True
  check (CppAssignment target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: Cpp -> String
targetVariable (CppVar var) = var
targetVariable (CppAccessor _ tgt) = targetVariable tgt
targetVariable (CppIndexer _ tgt) = targetVariable tgt
targetVariable _ = error "Invalid argument to targetVariable"

isUpdated :: String -> Cpp -> Bool
isUpdated var1 = everythingOnCpp (||) check
  where
  check :: Cpp -> Bool
  check (CppAssignment target _) | var1 == targetVariable target = True
  check _ = False

removeFromBlock :: ([Cpp] -> [Cpp]) -> Cpp -> Cpp
removeFromBlock go (CppBlock sts) = CppBlock (go sts)
removeFromBlock _  cpp = cpp
