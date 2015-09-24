-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.TCO
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements tail call elimination.
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Cpp.Optimizer.TCO (tco) where

import Data.List
import Data.Monoid

import Language.PureScript.Options
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.CodeGen.Cpp.AST

-- |
-- Eliminate tail calls
--
tco :: Options -> Cpp -> Cpp
tco opts | optionsNoTco opts = id
         | otherwise = tco'

tco' :: Cpp -> Cpp
tco' = everywhereOnCpp convert
  where
  tcoLabel :: String
  tcoLabel = "tco"
  tcoVar :: String -> String
  tcoVar arg = "__tco_" ++ arg
  copyVar :: (String, Maybe CppType) -> (String, Maybe CppType)
  copyVar (nm, ty) = ("__copy_" ++ nm, ty)
  convert :: Cpp -> Cpp
  convert cpp@(CppFunction name _ _ _ _) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id cpp
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = concat $ reverse argss
            in
              replace (toLoop name allArgs body')
        | otherwise -> cpp
  convert cpp@(CppVariableIntroduction (name, typ) qs (Just fn@CppLambda{})) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = concat $ reverse argss
            in
              CppVariableIntroduction (name, typ) qs (Just (replace (toLoop name allArgs body')))
        | otherwise -> cpp
  convert cpp = cpp
  collectAllFunctionArgs :: [[(String, Maybe CppType)]] -> (Cpp -> Cpp) -> Cpp -> ([[(String, Maybe CppType)]], Cpp, Cpp -> Cpp)
  collectAllFunctionArgs allArgs f (CppFunction ident args rty qs (CppBlock (body@(CppReturn _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (CppFunction ident (map copyVar args) rty qs (CppBlock [b]))) body
  collectAllFunctionArgs allArgs f (CppFunction ident args rty qs body@(CppBlock _)) =
    (args : allArgs, body, f . CppFunction ident (map copyVar args) rty qs)
  collectAllFunctionArgs allArgs f (CppLambda cps args rty (CppBlock (body@(CppReturn _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (CppLambda cps (map copyVar args) rty (CppBlock [b]))) body
  collectAllFunctionArgs allArgs f (CppLambda cps args rty body@(CppBlock _)) =
    (args : allArgs, body, f . CppLambda cps (map copyVar args) rty)
  collectAllFunctionArgs allArgs f (CppReturn (CppLambda cps args rty (CppBlock [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (CppReturn (CppLambda cps (map copyVar args) rty (CppBlock [b])))) body
  collectAllFunctionArgs allArgs f (CppReturn (CppLambda cps args rty body@(CppBlock _))) =
    (args : allArgs, body, f . CppReturn . CppLambda cps (map copyVar args) rty)
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)
  isTailCall :: String -> Cpp -> Bool
  isTailCall ident cpp =
    let
      numSelfCalls = everythingOnCpp (+) countSelfCalls cpp
      numSelfCallsInTailPosition = everythingOnCpp (+) countSelfCallsInTailPosition cpp
      numSelfCallsUnderFunctions = everythingOnCpp (+) countSelfCallsUnderFunctions cpp
      numSelfCallWithFnArgs = everythingOnCpp (+) countSelfCallsWithFnArgs cpp
    in
      numSelfCalls > 0
      && numSelfCalls == numSelfCallsInTailPosition
      && numSelfCallsUnderFunctions == 0
      && numSelfCallWithFnArgs == 0
    where
    countSelfCalls :: Cpp -> Int
    countSelfCalls (CppApp (CppVar ident') _) | ident == ident' = 1
    countSelfCalls _ = 0

    countSelfCallsInTailPosition :: Cpp -> Int
    countSelfCallsInTailPosition (CppReturn ret) | isSelfCall ident ret = 1
    countSelfCallsInTailPosition _ = 0

    countSelfCallsUnderFunctions (CppFunction _ _ _ _ cpp') = everythingOnCpp (+) countSelfCalls cpp'
    countSelfCallsUnderFunctions _ = 0

    countSelfCallsWithFnArgs :: Cpp -> Int
    countSelfCallsWithFnArgs ret = if isSelfCallWithFnArgs ident ret [] then 1 else 0

  toLoop :: String -> [(String, Maybe CppType)] -> Cpp -> Cpp
  toLoop ident allArgs cpp = CppBlock $
        map (\arg@(name, _) -> CppVariableIntroduction (name, Just $ CppAny []) [] (Just (CppVar (fst $ copyVar arg)))) allArgs ++
        [ CppLabel tcoLabel $ CppWhile (CppBooleanLiteral True) block ]
    where
    block :: Cpp
    block = case everywhereOnCpp loopify cpp of
              CppBlock sts -> CppBlock $ nub sts -- nub added to eliminate duplicate var introductions
              stmt -> CppBlock [stmt]
    loopify :: Cpp -> Cpp
    loopify (CppReturn ret) | isSelfCall ident ret =
      let
        allArgumentValues = concat $ collectSelfCallArgs [] ret
      in
        CppSequence $ zipWith (\val arg ->
                      CppVariableIntroduction (tcoVar arg, (Just $ CppAny [])) [] (Just val)) allArgumentValues (map fst allArgs)
                    ++ map (\arg ->
                      CppAssignment (CppVar arg) (CppVar (tcoVar arg))) (map fst allArgs)
                    ++ [ CppContinue tcoLabel ]
    loopify other = other
    collectSelfCallArgs :: [[Cpp]] -> Cpp -> [[Cpp]]
    collectSelfCallArgs allArgumentValues (CppApp fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues

  isSelfCall :: String -> Cpp -> Bool
  isSelfCall ident (CppApp (CppVar ident') args)
    | ident == ident' && not (any isFunction args) = True
  isSelfCall ident (CppApp fn args) | not (any isFunction args) = isSelfCall ident fn
  isSelfCall _ _ = False

  isFunction :: Cpp -> Bool
  isFunction (CppFunction _ _ _ _ _) = True
  isFunction _ = False

  isSelfCallWithFnArgs :: String -> Cpp -> [Cpp] -> Bool
  isSelfCallWithFnArgs ident (CppVar ident') args | ident == ident' && any hasFunction args = True
  isSelfCallWithFnArgs ident (CppApp fn args) acc = isSelfCallWithFnArgs ident fn (args ++ acc)
  isSelfCallWithFnArgs _ _ _ = False

  hasFunction :: Cpp -> Bool
  hasFunction = getAny . everythingOnCpp mappend (Any . isFunction)
