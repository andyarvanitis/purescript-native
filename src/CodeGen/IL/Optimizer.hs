-- | This module optimizes code in the intermediate representation.
module CodeGen.IL.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)
import Data.Text (Text)
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Blocks
import Language.PureScript.CoreImp.Optimizer.Common hiding (isDict)
import Language.PureScript.CoreImp.Optimizer.Inliner (etaConvert, evaluateIifes, inlineVariables, unThunk)
import Language.PureScript.CoreImp.Optimizer.Unused

import CodeGen.IL.Common (moduleNameToIL', undefinedName, unusedName)
import CodeGen.IL.Optimizer.Inliner
import CodeGen.IL.Optimizer.MagicDo
import CodeGen.IL.Optimizer.TCO

import qualified Language.PureScript.Constants as C

-- | Apply a series of optimizer passes to simplified IL code
optimize :: MonadSupply m => AST -> AST -> m AST
optimize mn il = do
    il' <- untilFixedPoint (inlineFnComposition . inlineUnsafeCoerce . inlineUnsafePartial . tidyUp . applyAll
      [ inlineCommonValues
      , inlineCommonOperators
      ]) il
    untilFixedPoint (return . tidyUp) . tco mn . inlineST
      =<< untilFixedPoint (return . magicDoST)
      =<< untilFixedPoint (return . magicDoEff)
      =<< untilFixedPoint (return . magicDoEffect) il'
  where
    tidyUp :: AST -> AST
    tidyUp = applyAll
      [ collapseNestedBlocks
      , collapseNestedIfs
      , collapseIfChecks
      , ignoreUnusedResults
      , removeCodeAfterReturnStatements
      -- , removeUndefinedApp
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

-- inlineApply :: AST -> AST
-- inlineApply = everywhereTopDown convert where
--   convert (App ss (App _ (Indexer _ (Var _ apply) (Var _ dataFunction)) [ f ]) [ arg ])
--     | apply == C.apply && dataFunction == C.dataFunction
--     = App ss f [ arg ]
--   convert (App ss (App _ (Indexer _ (Var _ applyFlipped) (Var _ dataFunction)) [ arg ]) [ f ])
--     | applyFlipped == C.applyFlipped && dataFunction == C.dataFunction
--     = App ss f [ arg ]
--   convert other = other

collapseIfChecks :: AST -> AST
collapseIfChecks = everywhere collapse where
  collapse :: AST -> AST
  collapse (IfElse _ (Binary _ EqualTo (BooleanLiteral _ True) (BooleanLiteral _ True)) (Block _ [exprs]) _) = exprs
  collapse (IfElse _ (Binary _ EqualTo (Indexer _ (Var _ prop) (Var _ val)) (BooleanLiteral _ True)) (Block _ [exprs]) _)
    | prop == "otherwise" && val == "Data_Boolean" = exprs
  collapse exprs = exprs


ignoreUnusedResults :: AST -> AST
ignoreUnusedResults = everywhere $ removeFromBlock go
  where
  go :: [AST] -> [AST]
  go [] = []
  go (VariableIntroduction ss var s@(Just _) : sts)
    | not $ any (everything (||) (isUsed var)) sts = (VariableIntroduction ss unusedName s) : sts
  go (s:sts) = s : go sts
