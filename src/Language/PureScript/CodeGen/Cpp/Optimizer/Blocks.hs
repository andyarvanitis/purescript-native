-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.Blocks
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Optimizer steps for simplifying C++ blocks
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Cpp.Optimizer.Blocks
  ( collapseNestedBlocks
  , collapseNestedIfs
  ) where

import Language.PureScript.CodeGen.Cpp.AST

-- |
-- Collapse blocks which appear nested directly below another block
--
collapseNestedBlocks :: Cpp -> Cpp
collapseNestedBlocks = everywhereOnCpp collapse
  where
  collapse :: Cpp -> Cpp
  collapse (CppBlock sts) = CppBlock (concatMap go sts)
  collapse cpp = cpp
  go :: Cpp -> [Cpp]
  go (CppBlock sts) = sts
  go s = [s]

collapseNestedIfs :: Cpp -> Cpp
collapseNestedIfs = everywhereOnCpp collapse
  where
  collapse :: Cpp -> Cpp
  collapse (CppIfElse cond1 (CppBlock [CppIfElse cond2 body Nothing]) Nothing) =
      CppIfElse (CppBinary And cond1 cond2) body Nothing
  collapse cpp = cpp
