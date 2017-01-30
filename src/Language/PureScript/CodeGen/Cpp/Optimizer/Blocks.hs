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
  , collapseIfElses
  , collapseCtorChecksToSwitch
  ) where

import Prelude.Compat
import Data.Maybe

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Types

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

collapseIfElses :: Cpp -> Cpp
collapseIfElses = everywhereOnCpp collapse
  where
  collapse :: Cpp -> Cpp
  collapse (CppBlock cpps) = CppBlock (go cpps)
    where
    go (st'@(CppIfElse (CppBinary EqualTo lhs _) _ Nothing) : sts') =
      if length (fst cpps') > 1
        then CppSwitch lhs (mkCases <$> fst cpps') Nothing : snd cpps'
        else st' : sts'
      where
      cpps' = span (isIntEq lhs) (st' : sts')
      isIntEq :: Cpp -> Cpp -> Bool
      isIntEq a (CppIfElse (CppBinary EqualTo a' (CppNumericLiteral (Left _))) (CppBlock [CppReturn _]) Nothing)
        | a == a' = True
      isIntEq a (CppIfElse (CppBinary EqualTo a' (CppCharLiteral _)) (CppBlock [CppReturn _]) Nothing)
        | a == a' = True
      isIntEq a (CppIfElse (CppBinary EqualTo a' (CppBooleanLiteral _)) (CppBlock [CppReturn _]) Nothing)
        | a == a' = True
      isIntEq _ _ = False
      mkCases :: Cpp -> (Cpp, Cpp)
      mkCases (CppIfElse (CppBinary EqualTo _ b') (CppBlock [body]) Nothing) = (b', body)
      mkCases (CppIfElse (CppUnary Not _) (CppBlock [body]) Nothing) = (CppBooleanLiteral False, body)
      mkCases (CppIfElse _ (CppBlock [body]) Nothing) = (CppBooleanLiteral True, body)
      mkCases _ = error ""
    go ((CppIfElse cond1 body1 Nothing) : (CppIfElse cond2 body2 Nothing) : _)
      | returns body1 && returns body2 &&
        (CppUnary Not cond1 == cond2 ||
         CppUnary Not cond2 == cond1) = [CppIfElse cond1 body1 (Just body2)]
      where
      returns :: Cpp -> Bool
      returns (CppBlock rets@(_:_)) | CppReturn{} <- last rets = True
      returns CppReturn{} = True
      returns _ = False
    go (cpp' : cpps') = cpp' : go cpps'
    go cpp' = cpp'
  collapse (CppIfElse (CppAccessor (CppVar "otherwise") (CppVar mn')) body _) | mn' == "Data_Boolean" = body
  collapse cpp = cpp

collapseCtorChecksToSwitch :: Cpp -> Cpp
collapseCtorChecksToSwitch = everywhereOnCpp collapse
  where
  collapse :: Cpp -> Cpp
  collapse (CppBlock cpps@(_:_:_)) = CppBlock (go cpps)
    where
    go cpps'@(cpp:_)
      | Just (lhs, _, _) <- fromif Nothing cpp
      , ifs@(_:_:_) <- catMaybes . takeWhile (isJust) $ fromif (Just lhs) <$> cpps'
      = CppSwitch lhs ((\(_,b,c) -> (b,c)) <$> ifs) Nothing : drop (length ifs) cpps'
    go (cpp':cpps') = cpp' : go cpps'
    go cpp' = cpp'
    fromif :: Maybe Cpp -> Cpp -> Maybe (Cpp, Cpp, Cpp)
    fromif cond (CppIfElse (CppBinary EqualTo lhs rhs) stmt Nothing)
      | CppApp (CppVar f) _ <- lhs
      , f == getCtor
      , maybe True (lhs==) cond
      = Just (lhs, rhs, stmt)
    fromif _ _ = Nothing
  collapse cpp = cpp
