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

import Prelude.Compat

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
  tcoVar :: String -> String
  tcoVar arg = "__tco_" ++ arg

  copyVar :: String -> String
  copyVar arg = "__copy_" ++ arg

  copyVar' :: (String, Maybe CppType) -> (String, Maybe CppType)
  copyVar' (arg, t) = (copyVar arg, t)

  convert :: Cpp -> Cpp

  convert cpp@(CppVariableIntroduction (name, ty) qs (Just fn@CppLambda {})) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = fst <$> (concat $ reverse argss)
            in
              CppVariableIntroduction (name, ty) qs (Just (replace (toLoop name allArgs body')))
        | otherwise -> cpp

  convert fn@(CppFunction name (_:_) _ _ _) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = fst <$> (concat $ reverse argss)
            in
              replace (toLoop name allArgs body')
        | otherwise -> fn

  convert cpp = cpp

  collectAllFunctionArgs :: [[(String, Maybe CppType)]] -> (Cpp -> Cpp) -> Cpp -> ([[(String, Maybe CppType)]], Cpp, Cpp -> Cpp)
  collectAllFunctionArgs allArgs f (CppFunction ident args rty qs (CppBlock (body@(CppReturn _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (CppFunction ident (map copyVar' args) rty qs (CppBlock [b]))) body
  collectAllFunctionArgs allArgs f (CppFunction ident args rty qs body@(CppBlock _)) =
    (args : allArgs, body, f . CppFunction ident (map copyVar' args) rty qs)
  collectAllFunctionArgs allArgs f (CppReturn (CppLambda cs args rty (CppBlock [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (CppReturn (CppLambda cs (map copyVar' args) rty (CppBlock [b])))) body
  collectAllFunctionArgs allArgs f (CppReturn (CppLambda cs args rty body@(CppBlock _))) =
    (args : allArgs, body, f . CppReturn . CppLambda cs (map copyVar' args) rty)
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

    countSelfCallsUnderFunctions :: Cpp -> Int
    countSelfCallsUnderFunctions (CppFunction _ _ _ _ cpp') = everythingOnCpp (+) countSelfCalls cpp'
    countSelfCallsUnderFunctions (CppLambda _ (_:_) _ cpp') = everythingOnCpp (+) countSelfCalls cpp'
    countSelfCallsUnderFunctions _ = 0

    countSelfCallsWithFnArgs :: Cpp -> Int
    countSelfCallsWithFnArgs ret = if isSelfCallWithFnArgs ident ret [] then 1 else 0

  toLoop :: String -> [String] -> Cpp -> Cpp
  toLoop ident allArgs cpp = CppBlock $
        map (\arg -> CppVariableIntroduction (arg, Just $ CppAny []) [] (Just (CppVar (copyVar arg)))) allArgs ++
        [ CppWhile (CppBooleanLiteral True) (CppBlock loop) ]
    where
    loop :: [Cpp]
    loop = case everywhereOnCpp loopify cpp of
             CppReturn (CppApp (CppLambda _ [] _ (CppBlock cpps')) []) -> nub cpps' -- elim dup var intros
             CppBlock cpps' -> nub cpps'
             cpp' -> [cpp']
    loopify :: Cpp -> Cpp
    loopify (CppReturn ret) | isSelfCall ident ret =
      let
        allArgumentValues = concat $ collectSelfCallArgs [] ret
      in
        CppBlock $ zipWith (\val arg ->
                    CppVariableIntroduction (tcoVar arg, Just $ CppAny []) [] (Just val)) allArgumentValues allArgs
                  ++ map (\arg ->
                    CppAssignment (CppVar arg) (CppVar (tcoVar arg))) allArgs
                  ++ [ CppContinue ]
    loopify other = other
    collectSelfCallArgs :: [[Cpp]] -> Cpp -> [[Cpp]]
    collectSelfCallArgs allArgumentValues (CppApp fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues

  isSelfCall :: String -> Cpp -> Bool
  isSelfCall ident (CppApp (CppVar ident') _) = ident == ident'
  isSelfCall ident (CppApp fn _) = isSelfCall ident fn
  isSelfCall _ _ = False

  isSelfCallWithFnArgs :: String -> Cpp -> [Cpp] -> Bool
  isSelfCallWithFnArgs ident (CppVar ident') args | ident == ident' && any hasFunction args = True
  isSelfCallWithFnArgs ident (CppApp fn args) acc = isSelfCallWithFnArgs ident fn (args ++ acc)
  isSelfCallWithFnArgs _ _ _ = False

  hasFunction :: Cpp -> Bool
  hasFunction = getAny . everythingOnCpp mappend (Any . isFunction)
    where
    isFunction (CppFunction {}) = True
    isFunction _ = False
