-- | This module performs basic inlining of known functions
module CodeGen.IL.Optimizer.Inliner
  ( inlineCommonValues
  , inlineCommonOperators
  , inlineFnComposition
  , inlineUnsafeCoerce
  , inlineUnsafePartial
  ) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)

import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.PSString (PSString, decodeString, mkString)
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Common hiding (isDict, isDict')
import Language.PureScript.AST (SourceSpan(..))
import qualified Language.PureScript.Constants as C

import CodeGen.IL.Common hiding (unbox)
import CodeGen.IL.Optimizer.Common

-- TODO: Potential bug:
-- Shouldn't just inline this case: { var x = 0; x.toFixed(10); }
-- Needs to be: { 0..toFixed(10); }
-- Probably needs to be fixed in pretty-printer instead.
shouldInline :: AST -> Bool
shouldInline (Var _ _) = True
shouldInline (NumericLiteral _ _) = True
shouldInline (StringLiteral _ _) = True
shouldInline (BooleanLiteral _ _) = True
shouldInline (Indexer _ index val) = shouldInline index && shouldInline val
shouldInline _ = False

inlineCommonValues :: AST -> AST
inlineCommonValues = everywhere convert
  where
  convert :: AST -> AST
  convert (App ss fn [dict])
    | isDict' [semiringNumber, semiringInt] dict && isDict fnZero fn = NumericLiteral ss (Left 0)
    | isDict' [semiringNumber, semiringInt] dict && isDict fnOne fn = NumericLiteral ss (Left 1)
    | isDict boundedBoolean dict && isDict fnBottom fn = BooleanLiteral ss False
    | isDict boundedBoolean dict && isDict fnTop fn = BooleanLiteral ss True
  convert (App ss (App _ fn [dict]) [x])
    | isDict ringInt dict && isDict fnNegate fn = Unary ss Negate (unbox x $ snd ringInt)
  convert (App ss (App _ (App _ fn [dict]) [x]) [y])
    | isDict semiringInt dict && isDict fnAdd fn = intOp ss Add semiringInt x y
    | isDict semiringInt dict && isDict fnMultiply fn = intOp ss Multiply semiringInt x y
    | isDict ringInt dict && isDict fnSubtract fn = intOp ss Subtract ringInt x y
  convert other = other
  fnZero = (C.dataSemiring, C.zero)
  fnOne = (C.dataSemiring, C.one)
  fnBottom = (C.dataBounded, C.bottom)
  fnTop = (C.dataBounded, C.top)
  fnAdd = (C.dataSemiring, C.add)
  fnMultiply = (C.dataSemiring, C.mul)
  fnSubtract = (C.dataRing, C.sub)
  fnNegate = (C.dataRing, C.negate)
  intOp ss op (_, d) x y = Binary ss op (unbox x d) (unbox y d)

inlineCommonOperators :: AST -> AST
inlineCommonOperators = everywhereTopDown $ applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate

  , binary euclideanRingNumber opDiv Divide

  -- , binary eqNumber opEq EqualTo
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

  , binary' C.dataIntBits C.or BitwiseOr
  , binary' C.dataIntBits C.and BitwiseAnd
  , binary' C.dataIntBits C.xor BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement BitwiseNot

  , inlineNonClassFunction (isModFn (C.dataFunction, C.apply)) $ \f x -> App Nothing f [x]
  , inlineNonClassFunction (isModFn (C.dataFunction, C.applyFlipped)) $ \x f -> App Nothing f [x]
  , inlineNonClassFunction (isModFnWithDict (C.dataArray, C.unsafeIndex)) $ flip (Indexer Nothing)
  ] -- ++
  -- [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ] ++
  -- [ fn | i <- [0..10], fn <- [ mkEffFn C.controlMonadEffUncurried C.mkEffFn i, runEffFn C.controlMonadEffUncurried C.runEffFn i ] ] ++
  -- [ fn | i <- [0..10], fn <- [ mkEffFn C.effectUncurried C.mkEffectFn i, runEffFn C.effectUncurried C.runEffectFn i ] ]
  where
  binary :: (Text, PSString) -> (Text, PSString) -> BinaryOperator -> AST -> AST
  binary dict@(_, d) fns op = convert where
    convert :: AST -> AST
    convert (App ss (App _ (App _ fn [dict']) [x]) [y])
      | isDict dict dict' && isDict fns fn = Binary ss op (unbox x d) (unbox y d)
    convert other = other
  binary' :: Text -> PSString -> BinaryOperator -> AST -> AST
  binary' moduleName opString op = convert where
    convert :: AST -> AST
    convert (App ss (App _ fn [x]) [y]) | isDict (moduleName, opString) fn = Binary ss op (unbox x mn) (unbox y mn)
    convert other = other
    mn :: PSString
    mn = mkString moduleName
  unary :: (Text, PSString) -> (Text, PSString) -> UnaryOperator -> AST -> AST
  unary dicts@(_, d) fns op = convert where
    convert :: AST -> AST
    convert (App ss (App _ fn [dict']) [x]) | isDict dicts dict' && isDict fns fn = Unary ss op (unbox x d)
    convert other = other
  unary' :: Text -> PSString -> UnaryOperator -> AST -> AST
  unary' moduleName fnName op = convert where
    convert :: AST -> AST
    convert (App ss fn [x]) | isDict (moduleName, fnName) fn = Unary ss op (unbox x fnName)
    convert other = other

  mkFn :: Int -> AST -> AST
  mkFn = mkFn' C.dataFunctionUncurried C.mkFn $ \ss1 ss2 ss3 args il ->
    Function ss1 Nothing args (Block ss2 [Return ss3 il])

  mkEffFn :: Text -> Text -> Int -> AST -> AST
  mkEffFn modName fnName = mkFn' modName fnName $ \ss1 ss2 ss3 args il ->
    Function ss1 Nothing args (Block ss2 [Return ss3 (App ss3 il [])])

  mkFn' :: Text -> Text -> (Maybe SourceSpan -> Maybe SourceSpan -> Maybe SourceSpan -> [Text] -> AST -> AST) -> Int -> AST -> AST
  mkFn' modName fnName res 0 = convert where
    convert :: AST -> AST
    convert (App _ mkFnN [Function s1 Nothing [_] (Block s2 [Return s3 il])]) | isNFn modName fnName 0 mkFnN =
      res s1 s2 s3 [] il
    convert other = other
  mkFn' modName fnName res n = convert where
    convert :: AST -> AST
    convert orig@(App ss mkFnN [fn]) | isNFn modName fnName n mkFnN =
      case collectArgs n [] fn of
        Just (args, [Return ss' ret]) -> res ss ss ss' args ret
        _ -> orig
    convert other = other
    collectArgs :: Int -> [Text] -> AST -> Maybe ([Text], [AST])
    collectArgs 1 acc (Function _ Nothing [oneArg] (Block _ il)) | length acc == n - 1 = Just (reverse (oneArg : acc), il)
    collectArgs m acc (Function _ Nothing [oneArg] (Block _ [Return _ ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: Text -> Text -> Int -> AST -> Bool
  isNFn expectMod prefix n (Indexer _ (Var _ name) (Var _ modName)) | modName == expectMod =
    name == fromString (T.unpack prefix <> show n)
  isNFn _ _ _ _ = False

  runFn :: Int -> AST -> AST
  runFn = runFn' C.dataFunctionUncurried C.runFn (\ss f args ->
                                                    let len = length args
                                                        typ = mkString $ "Fn" <> (T.pack . show $ len) in
                                                    if len > 0
                                                      then App ss (App Nothing (StringLiteral Nothing typ) [f]) args
                                                      else App ss f args)
  runEffFn :: Text -> Text -> Int -> AST -> AST
  runEffFn modName fnName = runFn' modName fnName $ \ss fn acc ->
    Function ss Nothing [] (Block ss [Return ss (App ss fn acc)])

  runFn' :: Text -> Text -> (Maybe SourceSpan -> AST -> [AST] -> AST) -> Int -> AST -> AST
  runFn' modName runFnName res n = convert where
    convert :: AST -> AST
    convert il = fromMaybe il $ go n [] il

    go :: Int -> [AST] -> AST -> Maybe AST
    go 0 acc (App ss runFnN [fn]) | isNFn modName runFnName n runFnN && length acc == n =
      Just $ res ss fn acc
    go m acc (App _ lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

  inlineNonClassFunction :: (AST -> Bool) -> (AST -> AST -> AST) -> AST -> AST
  inlineNonClassFunction p f = convert where
    convert :: AST -> AST
    convert (App _ (App _ op' [x]) [y]) | p op' = f x y
    convert other = other

  isModFn :: (Text, PSString) -> AST -> Bool
  isModFn (m, op) (Indexer _ (Var _ op') (Var _ m')) =
    m == m' && decodeString op == Just op'
  isModFn _ _ = False

  isModFnWithDict :: (Text, PSString) -> AST -> Bool
  isModFnWithDict (m, op) (App _ (Indexer _ (Var _ op') (Var _ m')) [Var _ _]) =
    m == m' && decodeString op == Just op'
  isModFnWithDict _ _ = False

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: forall m. MonadSupply m => AST -> m AST
inlineFnComposition = everywhereTopDownM convert where
  convert :: AST -> m AST
  convert (App s1 (App s2 (App _ (App _ fn [dict']) [x]) [y]) [z])
    | isFnCompose dict' fn = return $ App s1 x [App s2 y [z]]
    | isFnComposeFlipped dict' fn = return $ App s2 y [App s1 x [z]]
  convert app@(App ss (App _ (App _ fn [dict']) _) _)
    | isFnCompose dict' fn || isFnComposeFlipped dict' fn = mkApps ss <$> goApps app <*> freshName'
  convert other = return other

  mkApps :: Maybe SourceSpan -> [Either AST (Text, AST)] -> Text -> AST
  mkApps ss fns a = App ss (Function ss Nothing [] (Block ss $ vars <> [Return Nothing comp])) []
    where
    vars = uncurry (VariableIntroduction ss) . fmap Just <$> rights fns
    comp = Function ss Nothing [a] (Block ss [Return Nothing apps])
    apps = foldr (\fn acc -> App ss (mkApp fn) [acc]) (Var ss a) fns

  mkApp :: Either AST (Text, AST) -> AST
  mkApp = either id $ \(name, arg) -> Var (getSourceSpan arg) name

  goApps :: AST -> m [Either AST (Text, AST)]
  goApps (App _ (App _ (App _ fn [dict']) [x]) [y])
    | isFnCompose dict' fn = mappend <$> goApps x <*> goApps y
    | isFnComposeFlipped dict' fn = mappend <$> goApps y <*> goApps x
  goApps app@(App {}) = pure . Right . (,app) <$> freshName'
  goApps other = pure [Left other]

  isFnCompose :: AST -> AST -> Bool
  isFnCompose dict' fn = isDict semigroupoidFn dict' && isDict fnCompose fn

  isFnComposeFlipped :: AST -> AST -> Bool
  isFnComposeFlipped dict' fn = isDict semigroupoidFn dict' && isDict fnComposeFlipped fn

  fnCompose :: forall a b. (IsString a, IsString b) => (a, b)
  fnCompose = (C.controlSemigroupoid, C.compose)

  fnComposeFlipped :: forall a b. (IsString a, IsString b) => (a, b)
  fnComposeFlipped = (C.controlSemigroupoid, C.composeFlipped)

inlineUnsafeCoerce :: AST -> AST
inlineUnsafeCoerce = everywhereTopDown convert where
  convert (App _ (Indexer _ (Var _ unsafeCoerceFn) (Var _ unsafeCoerce)) [ comp ])
    | unsafeCoerceFn == C.unsafeCoerceFn && unsafeCoerce == C.unsafeCoerce
    = comp
  convert other = other

inlineUnsafePartial :: AST -> AST
inlineUnsafePartial = everywhereTopDown convert where
  convert (App ss fn [ comp ])
    | isDict fnUnsafePartial fn
    = App ss comp []
    where
      fnUnsafePartial :: forall a b. (IsString a, IsString b) => (a, b)
      fnUnsafePartial = (C.partialUnsafe, C.unsafePartial)
  convert other = other

semiringNumber :: forall a b. (IsString a, IsString b) => (a, b)
semiringNumber = (C.dataSemiring, C.semiringNumber)

semiringInt :: forall a b. (IsString a, IsString b) => (a, b)
semiringInt = (C.dataSemiring, C.semiringInt)

ringNumber :: forall a b. (IsString a, IsString b) => (a, b)
ringNumber = (C.dataRing, C.ringNumber)

ringInt :: forall a b. (IsString a, IsString b) => (a, b)
ringInt = (C.dataRing, C.ringInt)

euclideanRingNumber :: forall a b. (IsString a, IsString b) => (a, b)
euclideanRingNumber = (C.dataEuclideanRing, C.euclideanRingNumber)

eqNumber :: forall a b. (IsString a, IsString b) => (a, b)
eqNumber = (C.dataEq, C.eqNumber)

eqInt :: forall a b. (IsString a, IsString b) => (a, b)
eqInt = (C.dataEq, C.eqInt)

eqString :: forall a b. (IsString a, IsString b) => (a, b)
eqString = (C.dataEq, C.eqString)

eqChar :: forall a b. (IsString a, IsString b) => (a, b)
eqChar = (C.dataEq, C.eqChar)

eqBoolean :: forall a b. (IsString a, IsString b) => (a, b)
eqBoolean = (C.dataEq, C.eqBoolean)

ordBoolean :: forall a b. (IsString a, IsString b) => (a, b)
ordBoolean = (C.dataOrd, C.ordBoolean)

ordNumber :: forall a b. (IsString a, IsString b) => (a, b)
ordNumber = (C.dataOrd, C.ordNumber)

ordInt :: forall a b. (IsString a, IsString b) => (a, b)
ordInt = (C.dataOrd, C.ordInt)

ordString :: forall a b. (IsString a, IsString b) => (a, b)
ordString = (C.dataOrd, C.ordString)

ordChar :: forall a b. (IsString a, IsString b) => (a, b)
ordChar = (C.dataOrd, C.ordChar)

semigroupString :: forall a b. (IsString a, IsString b) => (a, b)
semigroupString = (C.dataSemigroup, C.semigroupString)

boundedBoolean :: forall a b. (IsString a, IsString b) => (a, b)
boundedBoolean = (C.dataBounded, C.boundedBoolean)

heytingAlgebraBoolean :: forall a b. (IsString a, IsString b) => (a, b)
heytingAlgebraBoolean = (C.dataHeytingAlgebra, C.heytingAlgebraBoolean)

semigroupoidFn :: forall a b. (IsString a, IsString b) => (a, b)
semigroupoidFn = (C.controlSemigroupoid, C.semigroupoidFn)

opAdd :: forall a b. (IsString a, IsString b) => (a, b)
opAdd = (C.dataSemiring, C.add)

opMul :: forall a b. (IsString a, IsString b) => (a, b)
opMul = (C.dataSemiring, C.mul)

opEq :: forall a b. (IsString a, IsString b) => (a, b)
opEq = (C.dataEq, C.eq)

opNotEq :: forall a b. (IsString a, IsString b) => (a, b)
opNotEq = (C.dataEq, C.notEq)

opLessThan :: forall a b. (IsString a, IsString b) => (a, b)
opLessThan = (C.dataOrd, C.lessThan)

opLessThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opLessThanOrEq = (C.dataOrd, C.lessThanOrEq)

opGreaterThan :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThan = (C.dataOrd, C.greaterThan)

opGreaterThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThanOrEq = (C.dataOrd, C.greaterThanOrEq)

opAppend :: forall a b. (IsString a, IsString b) => (a, b)
opAppend = (C.dataSemigroup, C.append)

opSub :: forall a b. (IsString a, IsString b) => (a, b)
opSub = (C.dataRing, C.sub)

opNegate :: forall a b. (IsString a, IsString b) => (a, b)
opNegate = (C.dataRing, C.negate)

opDiv :: forall a b. (IsString a, IsString b) => (a, b)
opDiv = (C.dataEuclideanRing, C.div)

opConj :: forall a b. (IsString a, IsString b) => (a, b)
opConj = (C.dataHeytingAlgebra, C.conj)

opDisj :: forall a b. (IsString a, IsString b) => (a, b)
opDisj = (C.dataHeytingAlgebra, C.disj)

opNot :: forall a b. (IsString a, IsString b) => (a, b)
opNot = (C.dataHeytingAlgebra, C.not)

unbox :: AST -> PSString -> AST
unbox e d = select . inlineCommonValues $ inlineCommonOperators e
  where
  select :: AST -> AST
  select e' =
    case e' of
      Var{} -> unbox' e'
      App{} -> unbox' e'
      Indexer{} -> unbox' e'
      _ -> e'
  unbox' :: AST -> AST
  unbox' e' = maybe e' (\t -> App Nothing (StringLiteral Nothing t) [e']) $ unboxType d

unboxType :: PSString -> Maybe PSString
unboxType s = do
  t <- decodeString s
  t' <- unboxType' t
  return $ mkString t'
  where
  unboxType' :: Text -> Maybe Text
  unboxType' t
    | t == C.eqInt ||
      t == C.semiringInt ||
      t == C.ringInt ||
      t == C.ordInt ||
      t == C.dataIntBits
      = Just int
    | t == C.semiringNumber ||
      t == C.ringNumber ||
      t == C.ordNumber ||
      t == C.euclideanRingNumber
      = Just float
    | t == C.eqBoolean ||
      t == C.ordBoolean ||
      t == C.heytingAlgebraBoolean
      = Just bool
    | t == C.eqChar ||
      t == C.eqString ||
      t == C.ordChar ||
      t == C.ordString ||
      t == C.semigroupString
      = Just string
    | otherwise = Nothing
