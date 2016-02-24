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

{-# LANGUAGE CPP #-}

module Language.PureScript.CodeGen.Cpp.Optimizer.Inliner (
  inlineVariables,
  inlineValues,
  inlineOperator,
  inlineCommonOperators,
  inlineFnComposition,
  etaConvert,
  unThunk,
  evaluateIifes
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative)
#endif
import Control.Monad.Supply.Class (MonadSupply, freshName)
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.CodeGen.Cpp.Optimizer.Common
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
  convert (CppApp (CppApp (CppApp fn [dict]) [x]) [y])
    | isDict semiringInt dict && isFn fnAdd fn = CppBinary Add x y
    | isDict semiringInt dict && isFn fnMultiply fn = CppBinary Multiply x y
    | isDict moduloSemiringInt dict && isFn fnDivide fn = CppBinary Divide x y
    | isDict ringInt dict && isFn fnSubtract fn = CppBinary Subtract x y
  convert other = other
  fnZero = (C.prelude, C.zero)
  fnOne = (C.prelude, C.one)
  fnBottom = (C.prelude, C.bottom)
  fnTop = (C.prelude, C.top)
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
  isOp (CppAccessor (CppVar longForm) (CppVar m')) = m == m' && longForm == identToCpp (Op op)
  isOp (CppIndexer (CppStringLiteral op') (CppVar m')) = m == m' && op == op'
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

  , binary booleanAlgebraBoolean (C.&&) And
  , binary booleanAlgebraBoolean (C.||) Or
  , binaryFunction booleanAlgebraBoolean C.conj And
  , binaryFunction booleanAlgebraBoolean C.disj Or
  , unary booleanAlgebraBoolean C.not CppNot

  , binary' C.dataIntBits (C..|.) BitwiseOr
  , binary' C.dataIntBits (C..&.) BitwiseAnd
  , binary' C.dataIntBits (C..^.) BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  -- , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement CppBitwiseNot
  ] -- ++ -- DISABLE mkFn/runFn inlining
  -- [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: (String, String) -> String -> BinaryOp -> Cpp -> Cpp
  binary dict opString op = everywhereOnCpp convert
    where
    convert :: Cpp -> Cpp
    convert (CppApp (CppApp (CppApp fn [dict']) [CppStringLiteral x]) [CppStringLiteral y])
      | isDict dict dict' && isPreludeFn opString fn = CppStringLiteral (x ++ y)
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
  -- mkFn :: Int -> Cpp -> Cpp
  -- mkFn 0 = everywhereOnCpp convert
  --   where
  --   convert :: Cpp -> Cpp
  --   convert (CppApp mkFnN [CppLambda caps [_] _ (CppBlock cpp)]) | isNFn C.mkFn 0 mkFnN =
  --     CppLambda caps [] Nothing (CppBlock cpp)
  --   convert other = other
  -- mkFn n = everywhereOnCpp convert
  --   where
  --   convert :: Cpp -> Cpp
  --   convert orig@(CppApp mkFnN [fn]) | isNFn C.mkFn n mkFnN =
  --     case collectArgs n [] fn of
  --       Just (args, cpp) -> CppLambda [CppCaptureAll] args Nothing (CppBlock cpp)
  --       Nothing -> orig
  --   convert other = other
  --   collectArgs :: Int -> [(String, Maybe CppType)] -> Cpp -> Maybe ([(String, Maybe CppType)], [Cpp])
  --   collectArgs 1 acc (CppLambda _ [oneArg] _ (CppBlock cpp)) | length acc == n - 1 = Just (reverse (oneArg : acc), cpp)
  --   collectArgs m acc (CppLambda _ [oneArg] _ (CppBlock [CppReturn ret])) = collectArgs (m - 1) (oneArg : acc) ret
  --   collectArgs _ _   _ = Nothing
  --
  -- isNFn :: String -> Int -> Cpp -> Bool
  -- isNFn prefix n (CppVar name) = name == (prefix ++ show n)
  -- isNFn prefix n (CppAccessor (CppVar name) (CppVar dataFunction)) | dataFunction == C.dataFunction = name == (prefix ++ show n)
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
inlineFnComposition :: (Applicative m, MonadSupply m) => Cpp -> m Cpp
inlineFnComposition = everywhereOnCppTopDownM convert
  where
  convert :: (MonadSupply m) => Cpp -> m Cpp
  convert (CppApp (CppApp (CppApp (CppApp fn [dict']) [x]) [y]) [z]) | isFnCompose dict' fn =
    return $ CppApp x [CppApp y [z]]
  convert (CppApp (CppApp (CppApp fn [dict']) [x]) [y]) | isFnCompose dict' fn = do
    arg <- freshName
    return $ CppLambda [CppCaptureAll] [(arg, Just (CppAny [CppConst, CppRef]))] (Just $ CppAny []) (CppBlock [CppReturn $ CppApp x [CppApp y [CppVar arg]]])
  convert other = return other
  isFnCompose :: Cpp -> Cpp -> Bool
  isFnCompose dict' fn = isDict semigroupoidFn dict' && (isPreludeFn (C.<<<) fn || isPreludeFn (C.compose) fn)

isDict :: (String, String) -> Cpp -> Bool
isDict (moduleName, dictName) (CppAccessor (CppVar x) (CppVar y)) = x == dictName && y == moduleName
isDict _ _ = False

isFn :: (String, String) -> Cpp -> Bool
isFn (moduleName, fnName) (CppAccessor (CppVar x) (CppVar y)) = x == (identToCpp (Ident fnName)) && y == moduleName
isFn (moduleName, fnName) (CppIndexer (CppStringLiteral x) (CppVar y)) = x == fnName && y == moduleName
isFn _ _ = False

isPreludeFn :: String -> Cpp -> Bool
isPreludeFn fnName = isFn (C.prelude, fnName)

semiringNumber :: (String, String)
semiringNumber = (C.prelude, C.semiringNumber)

semiringInt :: (String, String)
semiringInt = (C.prelude, C.semiringInt)

ringNumber :: (String, String)
ringNumber = (C.prelude, C.ringNumber)

ringInt :: (String, String)
ringInt = (C.prelude, C.ringInt)

moduloSemiringNumber :: (String, String)
moduloSemiringNumber = (C.prelude, C.moduloSemiringNumber)

moduloSemiringInt :: (String, String)
moduloSemiringInt = (C.prelude, C.moduloSemiringInt)

eqNumber :: (String, String)
eqNumber = (C.prelude, C.eqNumber)

eqInt :: (String, String)
eqInt = (C.prelude, C.eqInt)

eqString :: (String, String)
eqString = (C.prelude, C.eqNumber)

eqBoolean :: (String, String)
eqBoolean = (C.prelude, C.eqNumber)

ordNumber :: (String, String)
ordNumber = (C.prelude, C.ordNumber)

ordInt :: (String, String)
ordInt = (C.prelude, C.ordInt)

semigroupString :: (String, String)
semigroupString = (C.prelude, C.semigroupString)

boundedBoolean :: (String, String)
boundedBoolean = (C.prelude, C.boundedBoolean)

booleanAlgebraBoolean :: (String, String)
booleanAlgebraBoolean = (C.prelude, C.booleanAlgebraBoolean)

semigroupoidFn :: (String, String)
semigroupoidFn = (C.prelude, C.semigroupoidFn)
