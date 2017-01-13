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

import Prelude.Compat

import Data.Maybe (fromMaybe)
import Data.Text (Text, cons)

import Language.PureScript.CodeGen.Cpp.AST

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

replaceIdent :: Text -> Cpp -> Cpp -> Cpp
replaceIdent var1 cpp = everywhereOnCpp replace
  where
  replace (CppVar var2) | var1 == var2 = cpp
  replace other = other

replaceIdents :: [(Text, Cpp)] -> Cpp -> Cpp
replaceIdents vars = everywhereOnCpp replace
  where
  replace v@(CppVar var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: Text -> Cpp -> Bool
isReassigned var1 = everythingOnCpp (||) check
  where
  check :: Cpp -> Bool
  check (CppFunction _ args _ _ _) | var1 `elem` (map fst args) = True
  check (CppVariableIntroduction arg _ _) | var1 == fst arg = True
  check (CppAssignment (CppVar arg) _) | var1 == arg = True
  check _ = False

isRebound :: Cpp -> Cpp -> Bool
isRebound cpp d = any (\v -> isReassigned v d || isUpdated v d) (everythingOnCpp (++) variablesOf cpp)
  where
  variablesOf (CppVar var) = [var]
  variablesOf _ = []

isUsed :: Text -> Cpp -> Bool
isUsed var1 = everythingOnCpp (||) check
  where
  check :: Cpp -> Bool
  check (CppVar var2) | var1 == var2 = True
  check (CppAssignment target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: Cpp -> Text
targetVariable (CppVar var) = var
targetVariable (CppAccessor _ tgt) = targetVariable tgt
targetVariable (CppIndexer _ tgt) = targetVariable tgt
targetVariable (CppMapGet _ tgt) = targetVariable tgt
targetVariable _ = error "Invalid argument to targetVariable"

isUpdated :: Text -> Cpp -> Bool
isUpdated var1 = everythingOnCpp (||) check
  where
  check :: Cpp -> Bool
  check (CppAssignment target _) | var1 == targetVariable target = True
  check _ = False

removeFromBlock :: ([Cpp] -> [Cpp]) -> Cpp -> Cpp
removeFromBlock go (CppBlock sts) = CppBlock (go sts)
removeFromBlock _  cpp = cpp

isFn :: (Text, Text) -> Cpp -> Bool
isFn (moduleName, fnName) (CppAccessor (CppVar x) (CppVar y)) =
  (x == fnName || x == ('*' `cons` fnName)) && y == moduleName
isFn _ _ = False

isDict :: (Text, Text) -> Cpp -> Bool
isDict (moduleName, dictName) (CppAccessor (CppVar x) (CppVar y)) = x == dictName && y == moduleName
isDict _ _ = False

isDict' :: [(Text, Text)] -> Cpp -> Bool
isDict' xs cpp = any (`isDict` cpp) xs
