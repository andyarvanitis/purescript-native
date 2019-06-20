-- | This module optimizes code in the intermediate representation.
module CodeGen.IL.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)
import Data.Text (Text)
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Blocks
import Language.PureScript.CoreImp.Optimizer.Common hiding (isDict)
import Language.PureScript.CoreImp.Optimizer.Inliner hiding (inlineUnsafeCoerce, inlineUnsafePartial)
import Language.PureScript.CoreImp.Optimizer.Unused
import Language.PureScript.PSString (PSString, decodeString)

import CodeGen.IL.Common (unusedName)
import CodeGen.IL.Optimizer.TCO

import qualified Language.PureScript.Constants as C


-- | Apply a series of optimizer passes to simplified C++ code
optimize :: MonadSupply m => AST -> AST -> m AST
-- optimize = untilFixedPoint $ return . inlineUnsafeCoerce . inlineUnsafePartial . tidyUp . tco
optimize mn il = do
    il' <- untilFixedPoint (return . inlineApply . inlineUnsafeCoerce . inlineUnsafePartial . tidyUp) il
    untilFixedPoint (return . tidyUp) . tco mn
      =<< untilFixedPoint (return . magicDo')
      =<< untilFixedPoint (return . magicDo) il'


  where
    tidyUp :: AST -> AST
    tidyUp = applyAll
      [ collapseNestedBlocks
      -- , collapseNestedIfs
      , collapseIfChecks
      , removeCodeAfterReturnStatements
      , unThunk
      , etaConvert
      , evaluateIifes
      , inlineVariables
      ]

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'

-- | Overridden from CoreImp

inlineUnsafePartial :: AST -> AST
inlineUnsafePartial = everywhereTopDown convert where
  convert (App ss (Indexer _ (Var _ unsafePartial) (Var _ partialUnsafe)) [ comp ])
    | unsafePartial == C.unsafePartial && partialUnsafe == C.partialUnsafe
    -- Apply to undefined here, the application should be optimized away
    -- if it is safe to do so
    = App ss comp [ Var ss C.undefined ]
  convert other = other

inlineUnsafeCoerce :: AST -> AST
inlineUnsafeCoerce = everywhereTopDown convert where
  convert (App _ (Indexer _ (Var _ unsafeCoerceFn) (Var _ unsafeCoerce)) [ comp ])
    | unsafeCoerceFn == C.unsafeCoerceFn && unsafeCoerce == C.unsafeCoerce
    = comp
  convert other = other

inlineApply :: AST -> AST
inlineApply = everywhereTopDown convert where
  convert (App ss (App _ (Indexer _ (Var _ apply) (Var _ dataFunction)) [ f ]) [ arg ])
    | apply == C.apply && dataFunction == C.dataFunction
    = App ss f [ arg ]
  convert (App ss (App _ (Indexer _ (Var _ applyFlipped) (Var _ dataFunction)) [ arg ]) [ f ])
    | applyFlipped == C.applyFlipped && dataFunction == C.dataFunction
    = App ss f [ arg ]
  convert other = other

collapseIfChecks :: AST -> AST
collapseIfChecks = everywhere collapse where
  collapse :: AST -> AST
  collapse (IfElse _ (Binary _ EqualTo (BooleanLiteral _ True) (BooleanLiteral _ True)) (Block _ [exprs]) _) = exprs
  collapse (IfElse _ (Binary _ EqualTo (Indexer _ (Var _ prop) (Var _ val)) (BooleanLiteral _ True)) (Block _ [exprs]) _)
    | prop == "otherwise" && val == "Data_Boolean" = exprs
  collapse exprs = exprs

magicDo :: AST -> AST
magicDo = magicDo'' C.eff C.effDictionaries

magicDo' :: AST -> AST
magicDo' = magicDo'' C.effect C.effectDictionaries

magicDo'' :: Text -> C.EffectDictionaries -> AST -> AST
magicDo'' effectModule C.EffectDictionaries{..} = everywhereTopDown convert
  where
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: AST -> AST
  -- Desugar pure
  convert (App _ (App _ pure' [val]) []) | isPure pure' = val
  -- Desugar discard
  convert (App _ (App _ bind [m]) [Function s1 Nothing [unused] (Block s2 il)]) | isDiscard bind && unused == unusedName =
    Function s1 Nothing [] $ Block s2 (App s2 m [] : map applyReturns il )
  -- Desugar bind
  convert (App _ (App _ bind [m]) [Function s1 Nothing [arg] (Block s2 il)]) | isBind bind =
    Function s1 Nothing [] $ Block s2 (VariableIntroduction s2 arg (Just (App s2 m [])) : map applyReturns il)
  -- Inline double applications
  convert (App _ (App s1 (Function s2 Nothing [] (Block ss body)) []) []) =
    App s1 (Function s2 Nothing [] (Block ss (applyReturns `fmap` body))) []
  convert other = other

  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (App _ fn [dict]) | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a call to @discard@
  isDiscard (App _ (App _ fn [dict1]) [dict2])
    | isDict (C.controlBind, C.discardUnitDictionary) dict1 &&
      isDict (effectModule, edBindDict) dict2 &&
      isDiscardPoly fn = True
  isDiscard _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (App _ fn [dict]) | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isDict (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure function
  isPurePoly = isDict (C.controlApplicative, C.pure')
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isDict (C.controlBind, C.discard)


  applyReturns :: AST -> AST
  applyReturns (Return ss ret) = Return ss (App ss ret [])
  applyReturns (Block ss ils) = Block ss (map applyReturns ils)
  applyReturns (IfElse ss cond t f) = IfElse ss cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other

  isDict :: (Text, PSString) -> AST -> Bool
  isDict (moduleName, dictName) (Indexer _ (Var _ x) (Var _ y)) =
    Just x == decodeString dictName && y == moduleName
  isDict _ _ = False
