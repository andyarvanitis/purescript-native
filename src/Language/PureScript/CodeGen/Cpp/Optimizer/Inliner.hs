-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.Inliner
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module provides basic inlining capabilities
--
-----------------------------------------------------------------------------
module Language.PureScript.CodeGen.Cpp.Optimizer.Inliner
  ( inlineVariables
  , inlineCommonValues
  , inlineOperator
  , inlineCommonOperators
  , inlineFnComposition
  , etaConvert
  , unThunk
  , evaluateIifes
  , toAutoVars
  ) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.CodeGen.Cpp.Optimizer.Common
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C

-- TODO: Potential bug:
-- Shouldn't just inline this case: { var x = 0; x.toFixed(10); }
-- Needs to be: { 0..toFixed(10); }
-- Probably needs to be fixed in pretty-printer instead.
shouldInline :: Cpp -> Bool
shouldInline (CppVar _) = True
shouldInline (CppNumericLiteral _) = True
shouldInline (CppStringLiteral _) = True
shouldInline (CppBooleanLiteral _) = True
shouldInline (CppAccessor _ val) = shouldInline val
shouldInline (CppIndexer index val) = shouldInline index && shouldInline val
shouldInline (CppCast _ val) = shouldInline val
shouldInline _ = False

etaConvert :: Cpp -> Cpp
etaConvert = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppBlock [CppReturn (CppApp (CppLambda _ idents _ block@(CppBlock body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map (CppVar . fst) idents)) &&
      not (any (`isRebound` block) args)
      = CppBlock (map (replaceIdents (zip (map fst idents) args)) body)
  convert (CppLambda _ [] _ (CppBlock [CppReturn (CppApp fn [])])) = fn
  convert cpp = cpp

unThunk :: Cpp -> Cpp
unThunk = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppBlock []) = CppBlock []
  convert (CppBlock cpps) =
    case last cpps of
      CppReturn (CppApp (CppLambda _ [] _ (CppBlock body)) []) -> CppBlock $ init cpps ++ body
      _ -> CppBlock cpps
  convert cpp = cpp

evaluateIifes :: Cpp -> Cpp
evaluateIifes = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp (CppLambda _ [] _ (CppBlock [CppReturn ret])) []) = ret
  convert (CppReturn cpp@(CppApp _ _))
    | (f, args@(_:_)) <- unApp cpp [],
      CppApp (CppLambda _ [] _ (CppBlock cpps)) [] <- f,
      CppReturn ret <- last cpps = CppBlock $ init cpps ++ [CppReturn $foldl (\fn a -> CppApp fn [a]) ret args]
    where
    unApp :: Cpp -> [Cpp] -> (Cpp, [Cpp])
    unApp (CppApp f [arg]) args = unApp f (arg : args)
    unApp other args = (other, args)
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

inlineCommonValues :: Cpp -> Cpp
inlineCommonValues = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp fn [dict])
    | isDict' [semiringNumber, semiringInt] dict && isFn fnZero fn = CppNumericLiteral (Left 0)
    | isDict' [semiringNumber, semiringInt] dict && isFn fnOne fn = CppNumericLiteral (Left 1)
    | isDict boundedBoolean dict && isFn fnBottom fn = CppBooleanLiteral False
    | isDict boundedBoolean dict && isFn fnTop fn = CppBooleanLiteral True
  convert (CppApp fn [dict, x, y])
    | isDict semiringInt dict && isFn fnAdd fn = CppBinary Add x y
    | isDict semiringInt dict && isFn fnMultiply fn = CppBinary Multiply x y
    | isDict euclideanRingInt dict && isFn fnDivide fn = CppBinary Divide x y
    | isDict ringInt dict && isFn fnSubtract fn = CppBinary Subtract x y
  convert (CppApp (CppApp (CppApp fn [dict]) [x]) [y])
    | isDict semiringInt dict && isFn fnAdd fn = CppBinary Add x y
    | isDict semiringInt dict && isFn fnMultiply fn = CppBinary Multiply x y
    | isDict euclideanRingInt dict && isFn fnDivide fn = CppBinary Divide x y
    | isDict ringInt dict && isFn fnSubtract fn = CppBinary Subtract x y
  convert other = other
  fnZero = (C.dataSemiring, C.zero)
  fnOne = (C.dataSemiring, C.one)
  fnBottom = (C.dataBounded, C.bottom)
  fnTop = (C.dataBounded, C.top)
  fnAdd = (C.dataSemiring, C.add)
  fnDivide = (C.dataEuclideanRing, C.div)
  fnMultiply = (C.dataSemiring, C.mul)
  fnSubtract = (C.dataRing, C.sub)

inlineOperator :: (String, String) -> (Cpp -> Cpp -> Cpp) -> Cpp -> Cpp
inlineOperator (m, op) f = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert (CppApp op' [x, y]) | isOp op' = f x y
  convert (CppApp (CppApp op' [x]) [y]) | isOp op' = f x y
  convert other = other
  isOp (CppAccessor (CppVar longForm) (CppVar m')) = m == m' && longForm == identToCpp (Ident op)
  isOp (CppIndexer (CppStringLiteral op') (CppVar m')) = m == m' && op == op'
  isOp _ = False

inlineCommonOperators :: Cpp -> Cpp
inlineCommonOperators = applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate
  , binary ringInt opSub Subtract
  , unary  ringInt opNegate Negate

  , binary euclideanRingNumber opDiv Divide
  , binary euclideanRingInt opMod Modulus

  , binary eqNumber opEq EqualTo
  , binary eqNumber opNotEq NotEqualTo
  , binary eqInt opEq EqualTo
  , binary eqInt opNotEq NotEqualTo
  , binary eqString opEq EqualTo
  , binary eqString opNotEq NotEqualTo
  , binary eqChar opEq EqualTo
  , binary eqChar opNotEq NotEqualTo
  , binary eqBoolean opEq EqualTo
  , binary eqBoolean opNotEq NotEqualTo

  , binary ordBoolean opLessThan LessThan
  , binary ordBoolean opLessThanOrEq LessThanOrEqualTo
  , binary ordBoolean opGreaterThan GreaterThan
  , binary ordBoolean opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordChar opLessThan LessThan
  , binary ordChar opLessThanOrEq LessThanOrEqualTo
  , binary ordChar opGreaterThan GreaterThan
  , binary ordChar opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordInt opLessThan LessThan
  , binary ordInt opLessThanOrEq LessThanOrEqualTo
  , binary ordInt opGreaterThan GreaterThan
  , binary ordInt opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordNumber opLessThan LessThan
  , binary ordNumber opLessThanOrEq LessThanOrEqualTo
  , binary ordNumber opGreaterThan GreaterThan
  , binary ordNumber opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordString opLessThan LessThan
  , binary ordString opLessThanOrEq LessThanOrEqualTo
  , binary ordString opGreaterThan GreaterThan
  , binary ordString opGreaterThanOrEq GreaterThanOrEqualTo

  , binary semigroupString opAppend Add

  , binary heytingAlgebraBoolean opConj And
  , binary heytingAlgebraBoolean opDisj Or
  , unary  heytingAlgebraBoolean opNot Not

  , binary' C.dataIntBits (C.or) BitwiseOr
  , binary' C.dataIntBits (C.and) BitwiseAnd
  , binary' C.dataIntBits (C.xor) BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  -- , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement BitwiseNot
  ] ++
  []
  -- [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: (String, String) -> (String, String) -> BinaryOperator -> Cpp -> Cpp
  binary dict fns op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp fn [dict', x, y]) | isDict dict dict' && isFn fns fn = CppBinary op x y
    convert (CppApp (CppApp (CppApp fn [dict']) [x]) [y]) | isDict dict dict' && isFn fns fn = CppBinary op x y
    convert other = other
  binary' :: String -> String -> BinaryOperator -> Cpp -> Cpp
  binary' moduleName opString op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp fn [x, y]) | isFn (moduleName, opString) fn = CppBinary op x y
    convert (CppApp (CppApp fn [x]) [y]) | isFn (moduleName, opString) fn = CppBinary op x y
    convert other = other
  unary :: (String, String) -> (String, String) -> UnaryOperator -> Cpp -> Cpp
  unary dicts fns op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp fn [dict', x]) | isDict dicts dict' && isFn fns fn = CppUnary op x
    convert (CppApp (CppApp fn [dict']) [x]) | isDict dicts dict' && isFn fns fn = CppUnary op x
    convert other = other
  unary' :: String -> String -> UnaryOperator -> Cpp -> Cpp
  unary' moduleName fnName op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp fn [x]) | isFn (moduleName, fnName) fn = CppUnary op x
    convert other = other
  -- mkFn :: Int -> Cpp -> Cpp
  -- mkFn 0 = everywhereOnCpp convert
  --   where
  --   convert :: Cpp -> Cpp
  --   convert (CppApp mkFnN [CppLambda _ [_] _ (CppBlock cpp)]) | isNFn C.mkFn 0 mkFnN =
  --     CppLambda [CaptureAll] [] Nothing (CppBlock cpp)
  --   convert other = other
  -- mkFn n = everywhereOnCpp convert
  --   where
  --   convert :: Cpp -> Cpp
  --   convert orig@(CppApp mkFnN [fn]) | isNFn C.mkFn n mkFnN =
  --     case collectArgs n [] fn of
  --       Just (args, cpp) -> CppLambda [CaptureAll] args Nothing (CppBlock cpp)
  --       Nothing -> orig
  --   convert other = other
  --   collectArgs :: Int -> [String] -> Cpp -> Maybe ([String], [Cpp])
  --   collectArgs 1 acc (CppLambda _ [oneArg] _ (CppBlock cpp)) | length acc == n - 1 = Just (reverse (oneArg : acc), cpp)
  --   collectArgs m acc (CppLambda _ [oneArg] _ (CppBlock [CppReturn ret])) = collectArgs (m - 1) (oneArg : acc) ret
  --   collectArgs _ _   _ = Nothing
  --
  -- isNFn :: String -> Int -> Cpp -> Bool
  -- isNFn prefix n (CppVar name) = name == (prefix ++ show n)
  -- isNFn prefix n (CppAccessor _ name (CppVar dataFunction)) | dataFunction == C.dataFunction = name == (prefix ++ show n)
  -- isNFn _ _ _ = False
  --
  -- runFn :: Int -> Cpp -> Cpp
  -- runFn n = everywhereOnCpp convert
  --   where
  --   convert :: Cpp -> Cpp
  --   convert cpp = fromMaybe cpp $ go n [] cpp
  --
  --   go :: Int -> [Cpp] -> Cpp -> Maybe Cpp
  --   go 0 acc (CppApp runFnN [fn]) | isNFn C.runFn n runFnN && length acc == n = Just (CppApp fn acc)
  --   go m acc (CppApp lhs [arg]) = go (m - 1) (arg : acc) lhs
  --   go _ _   _ = Nothing

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: (MonadSupply m) => Cpp -> m Cpp
inlineFnComposition = everywhereOnCppTopDownM convert
  where
  convert :: (MonadSupply m) => Cpp -> m Cpp
  convert (CppApp fn [dict', x, y, z])
    | isFnCompose dict' fn = return $ CppApp  x [CppApp  y [z]]
    | isFnComposeFlipped dict' fn = return $ CppApp  y [CppApp  x [z]]
  convert (CppApp  (CppApp  (CppApp (CppApp fn [dict']) [x]) [y]) [z])
    | isFnCompose dict' fn = return $ CppApp  x [CppApp  y [z]]
    | isFnComposeFlipped dict' fn = return $ CppApp  y [CppApp  x [z]]
  convert (CppApp fn [dict', x, y])
    | isFnCompose dict' fn = do
        arg <- freshName
        return $ CppLambda [CaptureAll] [(arg, constAnyRef)] Nothing (CppBlock [CppReturn $ CppApp x [CppApp y [CppVar arg]]])
    | isFnComposeFlipped dict' fn = do
        arg <- freshName
        return $ CppLambda [CaptureAll] [(arg, constAnyRef)] Nothing (CppBlock [CppReturn $ CppApp y [CppApp x [CppVar arg]]])
  convert (CppApp (CppApp (CppApp fn [dict']) [x]) [y])
    | isFnCompose dict' fn = do
        arg <- freshName
        return $ CppLambda [CaptureAll] [(arg, constAnyRef)] Nothing (CppBlock [CppReturn $ CppApp x [CppApp y [CppVar arg]]])
    | isFnComposeFlipped dict' fn = do
        arg <- freshName
        return $ CppLambda [CaptureAll] [(arg, constAnyRef)] Nothing (CppBlock [CppReturn $ CppApp y [CppApp x [CppVar arg]]])
  convert other = return other
  isFnCompose :: Cpp -> Cpp -> Bool
  isFnCompose dict' fn = isDict semigroupoidFn dict' && isFn fnCompose fn
  isFnComposeFlipped :: Cpp -> Cpp -> Bool
  isFnComposeFlipped dict' fn = isDict semigroupoidFn dict' && isFn fnComposeFlipped fn
  fnCompose :: (String, String)
  fnCompose = (C.controlSemigroupoid, C.compose)
  fnComposeFlipped :: (String, String)
  fnComposeFlipped = (C.controlSemigroupoid, C.composeFlipped)

toAutoVars :: Cpp -> Cpp
toAutoVars = everywhereOnCpp convert
  where
  convert :: Cpp -> Cpp
  convert cpp@(CppVariableIntroduction (ident, (Just (Any tqs))) qs (Just value))
    | Static `notElem` qs,
      Ref `notElem` tqs =
      case value of
        CppNumericLiteral {} -> var auto'
        CppBooleanLiteral {} -> var auto'
        CppBinary {} -> var auto'
        CppUnary {} -> var auto'
        CppApp {} -> var auto'
        CppVar {} -> var autoref'
        CppAccessor (CppVar {}) _ -> var autoref'
        _ -> cpp
      where
      auto' = Auto tqs
      autoref' | Const `elem` tqs = Auto (Ref : tqs)
               | otherwise = auto'
      var t = CppVariableIntroduction (ident, (Just t)) qs (Just value)
  convert cpp = cpp

semiringNumber :: (String, String)
semiringNumber = (C.dataSemiring, C.semiringNumber)

semiringInt :: (String, String)
semiringInt = (C.dataSemiring, C.semiringInt)

ringNumber :: (String, String)
ringNumber = (C.dataRing, C.ringNumber)

ringInt :: (String, String)
ringInt = (C.dataRing, C.ringInt)

euclideanRingNumber :: (String, String)
euclideanRingNumber = (C.dataEuclideanRing, C.euclideanRingNumber)

euclideanRingInt :: (String, String)
euclideanRingInt = (C.dataEuclideanRing, C.euclideanRingInt)

eqNumber :: (String, String)
eqNumber = (C.dataEq, C.eqNumber)

eqInt :: (String, String)
eqInt = (C.dataEq, C.eqInt)

eqString :: (String, String)
eqString = (C.dataEq, C.eqString)

eqChar :: (String, String)
eqChar = (C.dataEq, C.eqChar)

eqBoolean :: (String, String)
eqBoolean = (C.dataEq, C.eqBoolean)

ordBoolean :: (String, String)
ordBoolean = (C.dataOrd, C.ordBoolean)

ordNumber :: (String, String)
ordNumber = (C.dataOrd, C.ordNumber)

ordInt :: (String, String)
ordInt = (C.dataOrd, C.ordInt)

ordString :: (String, String)
ordString = (C.dataOrd, C.ordString)

ordChar :: (String, String)
ordChar = (C.dataOrd, C.ordChar)

semigroupString :: (String, String)
semigroupString = (C.dataSemigroup, C.semigroupString)

boundedBoolean :: (String, String)
boundedBoolean = (C.dataBounded, C.boundedBoolean)

heytingAlgebraBoolean :: (String, String)
heytingAlgebraBoolean = (C.dataHeytingAlgebra, C.heytingAlgebraBoolean)

semigroupoidFn :: (String, String)
semigroupoidFn = (C.controlSemigroupoid, C.semigroupoidFn)

opAdd :: (String, String)
opAdd = (C.dataSemiring, C.add)

opMul :: (String, String)
opMul = (C.dataSemiring, C.mul)

opEq :: (String, String)
opEq = (C.dataEq, C.eq)

opNotEq :: (String, String)
opNotEq = (C.dataEq, C.notEq)

opLessThan :: (String, String)
opLessThan = (C.dataOrd, C.lessThan)

opLessThanOrEq :: (String, String)
opLessThanOrEq = (C.dataOrd, C.lessThanOrEq)

opGreaterThan :: (String, String)
opGreaterThan = (C.dataOrd, C.greaterThan)

opGreaterThanOrEq :: (String, String)
opGreaterThanOrEq = (C.dataOrd, C.greaterThanOrEq)

opAppend :: (String, String)
opAppend = (C.dataSemigroup, C.append)

opSub :: (String, String)
opSub = (C.dataRing, C.sub)

opNegate :: (String, String)
opNegate = (C.dataRing, C.negate)

opDiv :: (String, String)
opDiv = (C.dataEuclideanRing, C.div)

opMod :: (String, String)
opMod = (C.dataEuclideanRing, C.mod)

opConj :: (String, String)
opConj = (C.dataHeytingAlgebra, C.conj)

opDisj :: (String, String)
opDisj = (C.dataHeytingAlgebra, C.disj)

opNot :: (String, String)
opNot = (C.dataHeytingAlgebra, C.not)
