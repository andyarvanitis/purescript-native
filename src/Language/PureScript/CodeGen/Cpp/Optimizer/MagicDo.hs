-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.MagicDo
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Cpp.Optimizer.MagicDo (
  magicDo
) where

import Data.List (nub)
import Data.Maybe (fromJust, isJust)

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.Names
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C

magicDo :: Options mode -> Cpp -> Cpp
magicDo opts | optionsNoMagicDo opts = id
             | otherwise = inlineST . magicDo'

-- |
-- Inline type class dictionaries for >>= and return for the Eff monad
--
-- E.g.
--
--  Prelude[">>="](dict)(m1)(function(x) {
--    return ...;
--  })
--
-- becomes
--
--  function __do {
--    var x = m1();
--    ...
--  }
--
magicDo' :: Cpp -> Cpp
magicDo' = everywhereOnCpp undo . everywhereOnCppTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: Cpp -> Cpp
  -- Desugar return
  convert (CppApp (CppApp ret [val]) []) | isReturn ret = val
  -- Desugar pure
  convert (CppApp (CppApp pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  -- convert (CppApp (CppApp bind [m]) [CppLambda [] rty (CppBlock cpp)]) | isBind bind =
  --   CppFunction fnName [] [] rty [] $ CppBlock (CppApp m [] : map applyReturns cpp )
  -- Desugar >>=
  -- convert (CppApp (CppApp bind [m]) [CppLambda [arg] rty (CppBlock cpp)]) | isBind bind =
  --   CppFunction fnName [] [] rty [] $ CppBlock (CppVariableIntroduction arg (Just (CppApp m [])) : map applyReturns cpp)
  -- Desugar untilE
  convert (CppApp (CppApp f [arg]) []) | isEffFunc C.untilE f =
    CppApp (CppLambda [] Nothing (CppBlock [ CppWhile (CppUnary CppNot (CppApp arg [])) (CppBlock []), CppReturn $ CppObjectLiteral []])) []
  -- Desugar whileE
  convert (CppApp (CppApp (CppApp f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    CppApp (CppLambda [] Nothing (CppBlock [ CppWhile (CppApp arg1 []) (CppBlock [ CppApp arg2 [] ]), CppReturn $ CppObjectLiteral []])) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (CppApp bindPoly [effDict]) | isBindPoly bindPoly && isEffDict C.bindEffDictionary effDict = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to return for the Eff monad
  isReturn (CppApp retPoly [effDict]) | isRetPoly retPoly && isEffDict C.monadEffDictionary effDict = True
  isReturn _ = False
  -- Check if an expression represents a monomorphic call to pure for the Eff applicative
  isPure (CppApp purePoly [effDict]) | isPurePoly purePoly && isEffDict C.applicativeEffDictionary effDict = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly (CppAccessor _ prop (CppVar prelude)) = prelude == C.prelude && prop == identToCpp (Op (C.>>=))
  isBindPoly (CppAccessor _ prop (CppScope prelude)) = prelude == C.prelude && prop == identToCpp (Op (C.>>=))
  isBindPoly (CppIndexer (CppStringLiteral bind) (CppVar prelude)) = prelude == C.prelude && bind == (C.>>=)
  isBindPoly _ = False
  -- Check if an expression represents the polymorphic return function
  isRetPoly (CppAccessor _ returnEscaped (CppVar prelude)) = prelude == C.prelude && returnEscaped == C.returnEscaped
  isRetPoly (CppAccessor _ returnEscaped (CppScope prelude)) = prelude == C.prelude && returnEscaped == C.returnEscaped
  isRetPoly (CppIndexer (CppStringLiteral return') (CppVar prelude)) = prelude == C.prelude && return' == C.return
  isRetPoly _ = False
  -- Check if an expression represents the polymorphic pure function
  isPurePoly (CppAccessor _ pure' (CppVar prelude)) = prelude == C.prelude && pure' == C.pure'
  isPurePoly (CppAccessor _ pure' (CppScope prelude)) = prelude == C.prelude && pure' == C.pure'
  isPurePoly (CppIndexer (CppStringLiteral pure') (CppVar prelude)) = prelude == C.prelude && pure' == C.pure'
  isPurePoly _ = False
  -- Check if an expression represents a function in the Ef module
  isEffFunc name (CppAccessor _ name' (CppVar eff)) = eff == C.eff && name == name'
  isEffFunc name (CppAccessor _ name' (CppScope eff)) = eff == C.eff && name == name'
  isEffFunc _ _ = False
  -- Check if an expression represents the Monad Eff dictionary
  isEffDict name (CppVar ident) | ident == name = True
  isEffDict name (CppAccessor _ prop (CppVar eff)) = eff == C.eff && prop == name
  isEffDict name (CppAccessor _ prop (CppScope eff)) = eff == C.eff && prop == name
  isEffDict _ _ = False
  -- Remove __do function applications which remain after desugaring
  undo :: Cpp -> Cpp
  undo (CppReturn (CppApp (CppFunction ident _ [] _ _ body) [])) | ident == fnName = body
  undo other = other

  applyReturns :: Cpp -> Cpp
  applyReturns (CppReturn ret) = CppReturn (CppApp ret [])
  applyReturns (CppBlock cpps) = CppBlock (map applyReturns cpps)
  applyReturns (CppWhile cond cpp) = CppWhile cond (applyReturns cpp)
  applyReturns (CppFor v lo hi cpp) = CppFor v lo hi (applyReturns cpp)
  applyReturns (CppForIn v xs cpp) = CppForIn v xs (applyReturns cpp)
  applyReturns (CppIfElse cond t f) = CppIfElse cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other

-- |
-- Inline functions in the ST module
--
inlineST :: Cpp -> Cpp
inlineST = everywhereOnCpp convertBlock
  where
  -- Look for runST blocks and inline the STRefs there.
  -- If all STRefs are used in the scope of the same runST, only using { read, write, modify }STRef then
  -- we can be more aggressive about inlining, and actually turn STRefs into local variables.
  convertBlock (CppApp f [arg]) | isSTFunc C.runST f =
    let refs = nub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in everywhereOnCpp (convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg
  convertBlock other = other
  -- Convert a block in a safe way, preserving object wrappers of references,
  -- or in a more aggressive way, turning wrappers into local variables depending on the
  -- agg(ressive) parameter.
  convert agg (CppApp f [arg]) | isSTFunc C.newSTRef f =
   CppLambda [] Nothing (CppBlock [CppReturn $ if agg then arg else CppObjectLiteral [(C.stRefValue, arg)]])
  convert agg (CppApp (CppApp f [ref]) []) | isSTFunc C.readSTRef f =
    if agg then ref else CppAccessor Nothing C.stRefValue ref
  convert agg (CppApp (CppApp (CppApp f [ref]) [arg]) []) | isSTFunc C.writeSTRef f =
    if agg then CppAssignment ref arg else CppAssignment (CppAccessor Nothing C.stRefValue ref) arg
  convert agg (CppApp (CppApp (CppApp f [ref]) [func]) []) | isSTFunc C.modifySTRef f =
    if agg then CppAssignment ref (CppApp func [ref]) else  CppAssignment (CppAccessor Nothing C.stRefValue ref) (CppApp func [CppAccessor Nothing C.stRefValue ref])
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (CppAccessor _ name' (CppVar st)) = st == C.st && name == name'
  isSTFunc name (CppAccessor _ name' (CppScope st)) = st == C.st && name == name'
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everythingOnCpp (++) isSTRef
    where
    isSTRef (CppVariableIntroduction (ident, Nothing) _ (Just (CppApp (CppApp f [_]) [])))
      | isSTFunc C.newSTRef f = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to readSTRef, writeSTRef, modifySTRef
  findAllSTUsagesIn = everythingOnCpp (++) isSTUsage
    where
    isSTUsage (CppApp (CppApp f [ref]) []) | isSTFunc C.readSTRef f = [ref]
    isSTUsage (CppApp (CppApp (CppApp f [ref]) [_]) []) | isSTFunc C.writeSTRef f || isSTFunc C.modifySTRef f = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everythingOnCpp (++) isVar
    where
    isVar e@(CppVar v) | v == ref = [e]
    isVar _ = []
  -- Convert a Cpp value to a String if it is a CppVar
  toVar (CppVar v) = Just v
  toVar _ = Nothing
