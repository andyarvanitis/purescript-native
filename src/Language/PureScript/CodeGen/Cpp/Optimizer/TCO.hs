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
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.CodeGen.Cpp.Optimizer.TCO (tco) where

import Prelude.Compat

import Data.List
import Data.Monoid ((<>))
import qualified Data.Monoid as Monoid
import Data.Text (Text)

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
  tcoVar :: Text -> Text
  tcoVar arg = "tco$" <> arg

  copyVar :: Text -> Text
  copyVar arg = "copy$" <> arg

  copyVar' :: (Text, Maybe CppType) -> (Text, Maybe CppType)
  copyVar' (arg, t) = (copyVar arg, t)

  convert :: Cpp -> Cpp
  convert cpp@(CppVariableIntroduction (name, ty) qs (Just lambda@CppLambda {})) =
    let
      (argss, body', replace) = collectAllLambdaArgs [] id lambda
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = fst <$> (concat $ reverse argss)
            in
              CppVariableIntroduction (name, ty) qs (Just (replace (toLoop name allArgs body')))
        | otherwise -> cpp
  convert cpp@(CppFunction name _ _ _ _) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id cpp
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = fst <$> (concat $ reverse argss)
            in
              replace (toLoop name allArgs body')
        | otherwise -> cpp
  convert cpp = cpp

  collectAllLambdaArgs :: [[(Text, Maybe CppType)]] -> (Cpp -> Cpp) -> Cpp -> ([[(Text, Maybe CppType)]], Cpp, Cpp -> Cpp)
  collectAllLambdaArgs allArgs f (CppLambda cs args rty (CppBlock (body@(CppReturn _):_))) =
    collectAllLambdaArgs (args : allArgs) (\b -> f (CppLambda cs (map copyVar' args) rty (CppBlock [b]))) body
  collectAllLambdaArgs allArgs f (CppLambda cs args rty body@(CppBlock _)) =
    (args : allArgs, body, f . CppLambda cs (map copyVar' args) rty)
  collectAllLambdaArgs allArgs f (CppReturn (CppLambda cs args rty (CppBlock [body]))) =
    collectAllLambdaArgs (args : allArgs) (\b -> f (CppReturn (CppLambda cs (map copyVar' args) rty (CppBlock [b])))) body
  collectAllLambdaArgs allArgs f (CppReturn (CppLambda cs args rty body@(CppBlock _))) =
    (args : allArgs, body, f . CppReturn . CppLambda cs (map copyVar' args) rty)
  collectAllLambdaArgs allArgs f body = (allArgs, body, f)

  collectAllFunctionArgs :: [[(Text, Maybe CppType)]] -> (Cpp -> Cpp) -> Cpp -> ([[(Text, Maybe CppType)]], Cpp, Cpp -> Cpp)
  collectAllFunctionArgs allArgs f (CppFunction name args rty qs (CppBlock (body@(CppReturn _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (CppFunction name (map copyVar' args) rty qs (CppBlock [b]))) body
  collectAllFunctionArgs allArgs f (CppFunction name args rty qs body@(CppBlock _)) =
    (args : allArgs, body, f . CppFunction name (map copyVar' args) rty qs)
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)

  isTailCall :: Text -> Cpp -> Bool
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
    countSelfCallsUnderFunctions (CppLambda _ _ _ cpp') = everythingOnCpp (+) countSelfCalls cpp'
    countSelfCallsUnderFunctions _ = 0

    countSelfCallsWithFnArgs :: Cpp -> Int
    countSelfCallsWithFnArgs = go [] where
      go acc (CppVar ident')
        | ident == ident' && any hasFunction acc = 1
      go acc (CppApp fn args) = go (args ++ acc) fn
      go _ _ = 0

    hasFunction :: Cpp -> Bool
    hasFunction = Monoid.getAny . everythingOnCpp mappend (Monoid.Any . isFunction)
      where
      isFunction (CppFunction {}) = True
      isFunction (CppLambda {}) = True
      isFunction _ = False

  toLoop :: Text -> [Text] -> Cpp -> Cpp
  toLoop ident allArgs cpp = CppBlock $
        map (\arg -> CppVariableIntroduction (arg, alwaysType (Auto [])) [] (Just (CppVar (copyVar arg)))) allArgs ++
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
                    CppVariableIntroduction (tcoVar arg, alwaysType (Auto [Const])) [] (Just val)) allArgumentValues allArgs
                  ++ map (\arg ->
                    CppAssignment (CppVar arg) (CppVar (tcoVar arg))) allArgs
                  ++ [ CppContinue ]
    loopify other = other
    collectSelfCallArgs :: [[Cpp]] -> Cpp -> [[Cpp]]
    collectSelfCallArgs allArgumentValues (CppApp fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues

  isSelfCall :: Text -> Cpp -> Bool
  isSelfCall ident (CppApp (CppVar ident') _) = ident == ident'
  isSelfCall ident (CppApp fn _) = isSelfCall ident fn
  isSelfCall _ _ = False
