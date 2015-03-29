-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.Inliner
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module provides basic inlining capabilities
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Cpp.Optimizer.Inliner (
  inlineVariables,
  inlineValues,
  inlineOperator,
  inlineCommonOperators,
  etaConvert,
  unThunk,
  evaluateIifes
) where

import Data.Maybe (fromMaybe)

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.CodeGen.Cpp.Optimizer.Common
import Language.PureScript.CoreImp.Operators
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C

shouldInline :: Cpp -> Bool
shouldInline (CppVar _) = True
shouldInline (CppScope _) = True
shouldInline (CppNumericLiteral _) = True
shouldInline (CppStringLiteral _) = True
shouldInline (CppBooleanLiteral _) = True
shouldInline (CppAccessor _ val) = shouldInline val
shouldInline (CppIndexer index val) = shouldInline index && shouldInline val
shouldInline _ = False

etaConvert :: Cpp -> Cpp
etaConvert = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppBlock [CppReturn (CppApp (CppLambda idents block@(CppBlock body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map CppVar idents)) &&
      not (any (`isRebound` block) args)
      = CppBlock (map (replaceIdents (zip idents args)) body)
  convert (CppLambda [] (CppBlock [CppReturn (CppApp fn [])])) = fn
  convert cpp = cpp

unThunk :: Cpp -> Cpp
unThunk = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppBlock []) = CppBlock []
  convert (CppBlock cpps) =
    case last cpps of
      CppReturn (CppApp (CppLambda [] (CppBlock body)) []) -> CppBlock $ init cpps ++ body
      _ -> CppBlock cpps
  convert cpp = cpp

evaluateIifes :: Cpp -> Cpp
evaluateIifes = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp (CppLambda [] (CppBlock [CppReturn ret])) []) = ret
  convert cpp = cpp

inlineVariables :: Cpp -> Cpp
inlineVariables = everywhereOnCpp $ removeFromBlock go
  where
  go :: [Cpp] -> [Cpp]
  go [] = []
  go (CppVariableIntroduction var (Just cpp) : sts)
    | shouldInline cpp && not (any (isReassigned var) sts) && not (any (isRebound cpp) sts) && not (any (isUpdated var) sts) =
      go (map (replaceIdent var cpp) sts)
  go (s:sts) = s : go sts

inlineValues :: Cpp -> Cpp
inlineValues = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp fn [dict]) | isPreludeDict C.semiringNumber dict && isPreludeFn C.zero fn = CppNumericLiteral (Left 0)
  convert (CppApp fn [dict]) | isPreludeDict C.semiringNumber dict && isPreludeFn C.one fn = CppNumericLiteral (Left 1)
  convert (CppApp (CppApp fn [x]) [y]) | isPreludeFn (C.%) fn = CppBinary Modulus x y
  convert other = other

inlineOperator :: (String, String) -> (Cpp -> Cpp -> Cpp) -> Cpp -> Cpp
inlineOperator (m, op) f = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp (CppApp op' [x]) [y]) | isOp op' = f x y
  convert other = other
  isOp (CppAccessor longForm (CppScope m')) = m == m' && longForm == identToCpp (Op op)
  isOp (CppIndexer (CppStringLiteral op') (CppScope m')) = m == m' && op == op'
  isOp _ = False

inlineCommonOperators :: Cpp -> Cpp
inlineCommonOperators = applyAll $
  [ binary C.semiringNumber (C.+) Add
  , binary C.semiringNumber (C.*) Multiply
  , binary C.ringNumber (C.-) Subtract
  , unary  C.ringNumber C.negate CppNegate
  , binary C.moduloSemiringNumber (C./) Divide

  , binary C.ordNumber (C.<) LessThan
  , binary C.ordNumber (C.>) GreaterThan
  , binary C.ordNumber (C.<=) LessThanOrEqual
  , binary C.ordNumber (C.>=) GreaterThanOrEqual

  , binary C.eqNumber (C.==) Equal
  , binary C.eqNumber (C./=) NotEqual
  , binary C.eqString (C.==) Equal
  , binary C.eqString (C./=) NotEqual
  , binary C.eqBoolean (C.==) Equal
  , binary C.eqBoolean (C./=) NotEqual

  , binary C.semigroupString (C.<>) Add
  , binary C.semigroupString (C.++) Add

  , binaryFunction C.bitsNumber C.shl ShiftLeft
  , binaryFunction C.bitsNumber C.shr ShiftRight
  , binaryFunction C.bitsNumber C.zshr ZeroFillShiftRight
  , binary         C.bitsNumber (C..&.) BitwiseAnd
  , binary         C.bitsNumber (C..|.) BitwiseOr
  , binary         C.bitsNumber (C..^.) BitwiseXor
  , unary          C.bitsNumber C.complement CppBitwiseNot

  , binary C.boolLikeBoolean (C.&&) And
  , binary C.boolLikeBoolean (C.||) Or
  , unary  C.boolLikeBoolean C.not CppNot
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: String -> String -> BinaryOp -> Cpp -> Cpp
  binary dictName opString op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp (CppApp (CppApp fn [dict]) [x]) [y]) | isPreludeDict dictName dict && isPreludeFn opString fn = CppBinary op x y
    convert other = other
  binaryFunction :: String -> String -> BinaryOp -> Cpp -> Cpp
  binaryFunction dictName fnName op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp (CppApp (CppApp fn [dict]) [x]) [y]) | isPreludeFn fnName fn && isPreludeDict dictName dict = CppBinary op x y
    convert other = other
  unary :: String -> String -> CppUnaryOp -> Cpp -> Cpp
  unary dictName fnName op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp (CppApp fn [dict]) [x]) | isPreludeFn fnName fn && isPreludeDict dictName dict = CppUnary op x
    convert other = other
  mkFn :: Int -> Cpp -> Cpp
  mkFn 0 = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp mkFnN [CppLambda [_] (CppBlock cpp)]) | isNFn C.mkFn 0 mkFnN =
      CppLambda [] (CppBlock cpp)
    convert other = other
  mkFn n = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert orig@(CppApp mkFnN [fn]) | isNFn C.mkFn n mkFnN =
      case collectArgs n [] fn of
        Just (args, cpp) -> CppLambda args (CppBlock cpp)
        Nothing -> orig
    convert other = other
    collectArgs :: Int -> [String] -> Cpp -> Maybe ([String], [Cpp])
    collectArgs 1 acc (CppLambda [oneArg] (CppBlock cpp)) | length acc == n - 1 = Just (reverse (oneArg : acc), cpp)
    collectArgs m acc (CppLambda [oneArg] (CppBlock [CppReturn ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: String -> Int -> Cpp -> Bool
  isNFn prefix n (CppVar name) = name == (prefix ++ show n)
  isNFn prefix n (CppScope name) = name == (prefix ++ show n)
  isNFn prefix n (CppAccessor name (CppVar dataFunction)) | dataFunction == C.dataFunction = name == (prefix ++ show n)
  isNFn prefix n (CppAccessor name (CppScope dataFunction)) | dataFunction == C.dataFunction = name == (prefix ++ show n)
  isNFn _ _ _ = False

  runFn :: Int -> Cpp -> Cpp
  runFn n = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert cpp = fromMaybe cpp $ go n [] cpp

    go :: Int -> [Cpp] -> Cpp -> Maybe Cpp
    go 0 acc (CppApp runFnN [fn]) | isNFn C.runFn n runFnN && length acc == n = Just (CppApp fn acc)
    go m acc (CppApp lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

isPreludeDict :: String -> Cpp -> Bool
isPreludeDict dictName (CppInstance prelude _ prop _) = prelude == C.prelude && prop == dictName
isPreludeDict dictName (CppAccessor prop (CppScope prelude)) = prelude == C.prelude && prop == dictName
isPreludeDict _ _ = False

isPreludeFn :: String -> Cpp -> Bool
isPreludeFn fnName (CppInstance prelude _ fnName' _) = prelude == C.prelude && fnName' == fnName
isPreludeFn fnName (CppAccessor fnName' (CppScope prelude)) = prelude == C.prelude && fnName' == fnName
isPreludeFn fnName (CppIndexer (CppStringLiteral fnName') (CppScope prelude)) = prelude == C.prelude && fnName' == fnName
isPreludeFn fnName (CppAccessor longForm (CppAccessor prelude (CppVar _))) = prelude == C.prelude && longForm == identToCpp (Op fnName)
isPreludeFn fnName (CppAccessor longForm (CppAccessor prelude (CppScope _))) = prelude == C.prelude && longForm == identToCpp (Op fnName)
isPreludeFn _ _ = False
