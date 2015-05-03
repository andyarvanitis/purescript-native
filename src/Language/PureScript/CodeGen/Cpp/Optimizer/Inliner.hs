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

import Language.PureScript.CodeGen.Cpp.Types
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
shouldInline (CppAccessor _ _ val) = shouldInline val
shouldInline _ = False

etaConvert :: Cpp -> Cpp
etaConvert = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppBlock [CppReturn (CppApp (CppLambda idents _ block@(CppBlock body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map (CppVar . fst) idents)) &&
      not (any (`isRebound` block) args)
      = CppBlock (map (replaceIdents (zip (map fst idents) args)) body)
  convert (CppLambda [] _ (CppBlock [CppReturn (CppApp fn [])])) = fn
  convert cpp = cpp

unThunk :: Cpp -> Cpp
unThunk = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppBlock []) = CppBlock []
  convert (CppBlock cpps) =
    case last cpps of
      CppReturn (CppApp (CppLambda [] _ (CppBlock body)) []) -> CppBlock $ init cpps ++ body
      _ -> CppBlock cpps
  convert cpp = cpp

evaluateIifes :: Cpp -> Cpp
evaluateIifes = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp (CppLambda [] _ (CppBlock [CppReturn ret])) []) = ret
  convert cpp = cpp

inlineVariables :: Cpp -> Cpp
inlineVariables = everywhereOnCpp $ removeFromBlock go
  where
  go :: [Cpp] -> [Cpp]
  go [] = []
  go (CppVariableIntroduction (var, _) _ (Just cpp) : sts)
    | shouldInline cpp && not (any (isReassigned var) sts) && not (any (isRebound cpp) sts) && not (any (isUpdated var) sts) =
      go (map (replaceIdent var cpp) sts)
  go (s:sts) = s : go sts

inlineValues :: Cpp -> Cpp
inlineValues = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp fn [dict]) | isDict semiringNumber dict && isFn fnZero fn = CppNumericLiteral (Left 0)
                            | isDict semiringNumber dict && isFn fnOne fn = CppNumericLiteral (Left 1)
                            | isDict semiringInt dict && isFn fnZero fn = CppNumericLiteral (Left 0)
                            | isDict semiringInt dict && isFn fnOne fn = CppNumericLiteral (Left 1)
                            | isDict boundedBoolean dict && isFn fnBottom fn = CppBooleanLiteral False
                            | isDict boundedBoolean dict && isFn fnTop fn = CppBooleanLiteral True
  convert (CppApp fn [value]) | isFn fromNumber fn = CppBinary BitwiseOr value (CppNumericLiteral (Left 0))
  convert (CppApp (CppApp (CppApp fn [dict]) [x]) [y])
    | isDict semiringInt dict && isFn fnAdd fn = CppBinary BitwiseOr (CppBinary Add x y) (CppNumericLiteral (Left 0))
    | isDict semiringInt dict && isFn fnMultiply fn = CppBinary BitwiseOr (CppBinary Multiply x y) (CppNumericLiteral (Left 0))
    | isDict moduloSemiringInt dict && isFn fnDivide fn = CppBinary BitwiseOr (CppBinary Divide x y) (CppNumericLiteral (Left 0))
    | isDict ringInt dict && isFn fnSubtract fn = CppBinary BitwiseOr (CppBinary Subtract x y) (CppNumericLiteral (Left 0))
  convert other = other
  fnZero = (C.prelude, C.zero)
  fnOne = (C.prelude, C.one)
  fnBottom = (C.prelude, C.bottom)
  fnTop = (C.prelude, C.top)
  fromNumber = (C.dataInt, C.fromNumber)
  fnAdd = (C.prelude, (C.+))
  fnDivide = (C.prelude, (C./))
  fnMultiply = (C.prelude, (C.*))
  fnSubtract = (C.prelude, (C.-))

inlineOperator :: (String, String) -> (Cpp -> Cpp -> Cpp) -> Cpp -> Cpp
inlineOperator (m, op) f = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp (CppApp op' [x]) [y]) | isOp op' = f x y
  convert other = other
  isOp (CppAccessor _ longForm (CppScope m')) = m == m' && longForm == identToCpp (Op op)
  isOp _ = False

inlineCommonOperators :: Cpp -> Cpp
inlineCommonOperators = applyAll $
  [ binary semiringNumber (C.+) Add
  , binary semiringNumber (C.*) Multiply

  , binary ringNumber (C.-) Subtract
  , unary  ringNumber C.negate CppNegate
  , binary ringInt (C.-) Subtract
  , unary  ringInt C.negate CppNegate

  , binary moduloSemiringNumber (C./) Divide
  , binary moduloSemiringInt C.mod Modulus

  , binary eqNumber (C.==) Equal
  , binary eqNumber (C./=) NotEqual
  , binary eqInt (C.==) Equal
  , binary eqInt (C./=) NotEqual
  , binary eqString (C.==) Equal
  , binary eqString (C./=) NotEqual
  , binary eqBoolean (C.==) Equal
  , binary eqBoolean (C./=) NotEqual

  , binary ordNumber (C.<) LessThan
  , binary ordNumber (C.>) GreaterThan
  , binary ordNumber (C.<=) LessThanOrEqual
  , binary ordNumber (C.>=) GreaterThanOrEqual
  , binary ordInt (C.<) LessThan
  , binary ordInt (C.>) GreaterThan
  , binary ordInt (C.<=) LessThanOrEqual
  , binary ordInt (C.>=) GreaterThanOrEqual

  , binary semigroupString (C.<>) Add
  , binary semigroupString (C.++) Add

  , binary latticeBoolean (C.&&) And
  , binary latticeBoolean (C.||) Or
  , binaryFunction latticeBoolean C.inf And
  , binaryFunction latticeBoolean C.sup Or
  , unary complementedLatticeBoolean C.not CppNot

  , binary' C.dataIntBits (C..|.) BitwiseOr
  , binary' C.dataIntBits (C..&.) BitwiseAnd
  , binary' C.dataIntBits (C..^.) BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement CppBitwiseNot
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: (String, String) -> String -> BinaryOp -> Cpp -> Cpp
  binary dict opString op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp (CppApp (CppApp fn [dict']) [x]) [y]) | isDict dict dict' && isPreludeFn opString fn = CppBinary op x y
    convert other = other
  binary' :: String -> String -> BinaryOp -> Cpp -> Cpp
  binary' moduleName opString op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp (CppApp fn [x]) [y]) | isFn (moduleName, opString) fn = CppBinary op x y
    convert other = other
  binaryFunction :: (String, String) -> String -> BinaryOp -> Cpp -> Cpp
  binaryFunction dict fnName op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp (CppApp (CppApp fn [dict']) [x]) [y]) | isPreludeFn fnName fn && isDict dict dict' = CppBinary op x y
    convert other = other
  unary :: (String, String) -> String -> CppUnaryOp -> Cpp -> Cpp
  unary dict fnName op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp (CppApp fn [dict']) [x]) | isPreludeFn fnName fn && isDict dict dict' = CppUnary op x
    convert other = other
  unary' :: String -> String -> CppUnaryOp -> Cpp -> Cpp
  unary' moduleName fnName op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp fn [x]) | isFn (moduleName, fnName) fn = CppUnary op x
    convert other = other
  mkFn :: Int -> Cpp -> Cpp
  mkFn 0 = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp mkFnN [CppLambda [_] _ (CppBlock cpp)]) | isNFn C.mkFn 0 mkFnN =
      CppLambda [] Nothing (CppBlock cpp)
    convert other = other
  mkFn n = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert orig@(CppApp mkFnN [fn]) | isNFn C.mkFn n mkFnN =
      case collectArgs n [] fn of
        Just (args, cpp) -> CppLambda args Nothing (CppBlock cpp)
        Nothing -> orig
    convert other = other
    collectArgs :: Int -> [(String, Maybe Type)] -> Cpp -> Maybe ([(String, Maybe Type)], [Cpp])
    collectArgs 1 acc (CppLambda [oneArg] Nothing (CppBlock cpp)) | length acc == n - 1 = Just (reverse (oneArg : acc), cpp)
    collectArgs m acc (CppLambda [oneArg] Nothing (CppBlock [CppReturn ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: String -> Int -> Cpp -> Bool
  isNFn prefix n (CppVar name) = name == (prefix ++ show n)
  isNFn prefix n (CppScope name) = name == (prefix ++ show n)
  isNFn prefix n (CppAccessor _ name (CppVar dataFunction)) | dataFunction == C.dataFunction = name == (prefix ++ show n)
  isNFn prefix n (CppAccessor _ name (CppScope dataFunction)) | dataFunction == C.dataFunction = name == (prefix ++ show n)
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


-- -- (f <<< g $ x) = f (g x)
-- -- (f <<< g)     = \x -> f (g x)
-- inlineArrComposition :: (Applicative m, MonadSupply m) => Cpp -> m Cpp
-- inlineArrComposition = everywhereOnCppTopDownM convert
--   where
--   convert :: (MonadSupply m) => Cpp -> m Cpp
--   convert (CppApp (CppApp (CppApp (CppApp fn [dict']) [x]) [y]) [z]) | isArrCompose dict' fn =
--     return $ CppApp x [CppApp y [z]]
--   convert (CppApp (CppApp (CppApp fn [dict']) [x]) [y]) | isArrCompose dict' fn = do
--     arg <- freshName
--     return $ CppFunction Nothing [arg] (CppBlock [CppReturn $ CppApp x [CppApp y [CppVar arg]]])
--   convert other = return other
--   isArrCompose :: Cpp -> Cpp -> Bool
--   isArrCompose dict' fn = isDict semigroupoidArr dict' && isPreludeFn (C.<<<) fn

isDict :: (String, String) -> Cpp -> Bool
isDict (moduleName, dictName) (CppAccessor _ x (CppVar y)) = x == dictName && y == moduleName
isDict _ _ = False

isFn :: (String, String) -> Cpp -> Bool
isFn (moduleName, fnName) (CppAccessor _ x (CppVar y)) = x == fnName && y == moduleName
isFn (moduleName, fnName) (CppIndexer (CppStringLiteral x) (CppVar y)) = x == fnName && y == moduleName
isFn _ _ = False

isPreludeFn :: String -> Cpp -> Bool
isPreludeFn fnName (CppInstance prelude _ fnName' _) = prelude == C.prelude && fnName' == fnName
isPreludeFn fnName (CppAccessor _ fnName' (CppScope prelude)) = prelude == C.prelude && fnName' == fnName
isPreludeFn fnName (CppAccessor _ longForm (CppAccessor _ prelude (CppVar _))) = prelude == C.prelude && longForm == identToCpp (Op fnName)
isPreludeFn fnName (CppAccessor _ longForm (CppAccessor _ prelude (CppScope _))) = prelude == C.prelude && longForm == identToCpp (Op fnName)
isPreludeFn _ _ = False

semiringNumber :: (String, String)
semiringNumber = (C.prelude, C.semiringNumber)

semiringInt :: (String, String)
semiringInt = (C.dataInt, C.semiringInt)

ringNumber :: (String, String)
ringNumber = (C.prelude, C.ringNumber)

ringInt :: (String, String)
ringInt = (C.dataInt, C.ringInt)

moduloSemiringNumber :: (String, String)
moduloSemiringNumber = (C.prelude, C.moduloSemiringNumber)

moduloSemiringInt :: (String, String)
moduloSemiringInt = (C.dataInt, C.moduloSemiringInt)

eqNumber :: (String, String)
eqNumber = (C.prelude, C.eqNumber)

eqInt :: (String, String)
eqInt = (C.dataInt, C.eqInt)

eqString :: (String, String)
eqString = (C.prelude, C.eqNumber)

eqBoolean :: (String, String)
eqBoolean = (C.prelude, C.eqNumber)

ordNumber :: (String, String)
ordNumber = (C.prelude, C.ordNumber)

ordInt :: (String, String)
ordInt = (C.dataInt, C.ordInt)

semigroupString :: (String, String)
semigroupString = (C.prelude, C.semigroupString)

boundedBoolean :: (String, String)
boundedBoolean = (C.prelude, C.boundedBoolean)

latticeBoolean :: (String, String)
latticeBoolean = (C.prelude, C.latticeBoolean)

complementedLatticeBoolean :: (String, String)
complementedLatticeBoolean = (C.prelude, C.complementedLatticeBoolean)

-- semigroupoidArr :: (String, String)
-- semigroupoidArr = (C.prelude, C.semigroupoidArr)
