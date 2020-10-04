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

import CodeGen.IL.Common (unusedName)
import CodeGen.IL.Optimizer.Inliner
import CodeGen.IL.Optimizer.MagicDo
import CodeGen.IL.Optimizer.TCO

import qualified Language.PureScript.Constants as C

-- | Apply a series of optimizer passes to simplified IL code
optimize :: MonadSupply m => AST -> AST -> m AST
optimize mn il = do
    il' <- untilFixedPoint (inlineFnComposition . inlineUnsafeCoerce . inlineUnsafePartial . tidyUp) il
    il'' <- untilFixedPoint (return . ignoreUnusedResults . inlineCommonValues . inlineCommonOperators . tidyUp) . tco mn . inlineST
      =<< untilFixedPoint (return . magicDoST)
      =<< untilFixedPoint (return . magicDoEff)
      =<< untilFixedPoint (return . magicDoEffect) il'
    untilFixedPoint (return . collapseVarDeclarations) il''
  where
    tidyUp :: AST -> AST
    tidyUp = applyAll
      [ collapseNestedBlocks
      , collapseNestedIfs
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

collapseIfChecks :: AST -> AST
collapseIfChecks = everywhere collapse where
  collapse :: AST -> AST
  collapse (IfElse _ (Binary _ EqualTo (BooleanLiteral _ True) (BooleanLiteral _ True)) (Block _ [exprs]) _) = exprs
  collapse (IfElse _ (Binary _ EqualTo (Indexer _ (Var _ prop) (Var _ val)) (BooleanLiteral _ True)) (Block _ [exprs]) _)
    | prop == "otherwise" && val == "Data_Boolean" = exprs
  collapse exprs = exprs

collapseVarDeclarations :: AST -> AST
collapseVarDeclarations = everywhere collapse where
  collapse :: AST -> AST
  collapse (Block ss sts) = Block ss (go Nothing sts)
  collapse s = s

  go :: Maybe Text -> [AST] -> [AST]
  go _ [] = []
  go Nothing (decl@(VariableIntroduction _ var (Just _)) : sts) = decl : go (Just var) (go Nothing sts)
  go (Just var') ((VariableIntroduction ss var (Just val)) : sts)
    | var == var' = (Assignment ss (Var Nothing var) val) : go (Just var') sts
  go var' (s:sts) = s : go var' sts

ignoreUnusedResults :: AST -> AST
ignoreUnusedResults = everywhere $ removeFromBlock go
  where
  go :: [AST] -> [AST]
  go [] = []
  go (VariableIntroduction ss var (Just s) : sts)
    | not $ any (everything (||) (isUsed var)) sts = sts'
    where
    sts' | App{} <- s = s : (go sts)
         | otherwise = go sts
  go (s:sts) = s : go sts
