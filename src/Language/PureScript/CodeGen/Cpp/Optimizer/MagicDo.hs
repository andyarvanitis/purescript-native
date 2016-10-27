-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.MagicDo
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
-----------------------------------------------------------------------------
module Language.PureScript.CodeGen.Cpp.Optimizer.MagicDo (magicDo) where

import Prelude.Compat

import Data.List (nub)
import Data.Maybe (fromJust, isJust)

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Optimizer.Common
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C

magicDo :: Options -> Cpp -> Cpp
magicDo opts | optionsNoMagicDo opts = id
             | otherwise = {- inlineST . -} magicDo' -- TODO: fix and test with Collatz.purs

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
  -- Desugar pure & return
  convert (CppApp (CppApp pure' [dict, val]) []) | isPure (CppApp pure' [dict]) = val
  convert (CppApp (CppApp pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  convert (CppApp bind [dict, m, CppLambda _ [] rty (CppBlock cpp)]) | isBind (CppApp bind [dict]) =
    CppFunction fnName [] rty [] $ CppBlock (CppApp m [] : map applyReturns cpp )
  convert (CppApp (CppApp bind [m]) [CppLambda _ [] rty (CppBlock cpp)]) | isBind bind =
    CppFunction fnName [] rty [] $ CppBlock (CppApp m [] : map applyReturns cpp )
  -- Desugar >>=
  convert (CppApp bind [dict, m, CppLambda _ [arg] rty (CppBlock cpp)]) | isBind (CppApp bind [dict]) =
    CppFunction fnName [] rty [] $ CppBlock (app' : map applyReturns cpp)
    where
      app' | fst arg == C.__unused = CppApp m []
           | otherwise = CppVariableIntroduction arg [] (Just (CppApp m []))
  convert (CppApp (CppApp bind [m]) [CppLambda _ [arg] rty (CppBlock cpp)]) | isBind bind =
    CppFunction fnName [] rty [] $ CppBlock (app' : map applyReturns cpp)
    where
      app' | fst arg == C.__unused = CppApp m []
           | otherwise = CppVariableIntroduction arg [] (Just (CppApp m []))
  -- Desugar untilE
  convert (CppApp (CppApp f [arg]) []) | isEffFunc C.untilE f =
    CppApp (CppLambda [CppCaptureAll] [] Nothing (CppBlock [ CppWhile (CppUnary Not (CppApp arg [])) (CppBlock []), CppReturn $ CppObjectLiteral CppRecord []])) []
  -- Desugar whileE
  convert (CppApp (CppApp f [arg1, arg2]) []) | isEffFunc C.whileE f =
    CppApp (CppLambda [CppCaptureAll] [] Nothing (CppBlock [ CppWhile (CppApp arg1 []) (CppBlock [ CppApp arg2 [] ]), CppReturn $ CppObjectLiteral CppRecord []])) []
  convert (CppApp (CppApp (CppApp f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    CppApp (CppLambda [CppCaptureAll] [] Nothing (CppBlock [ CppWhile (CppApp arg1 []) (CppBlock [ CppApp arg2 [] ]), CppReturn $ CppObjectLiteral CppRecord []])) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (CppApp fn [dict]) | isDict (C.eff, C.bindEffDictionary) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (CppApp fn [dict]) | isDict (C.eff, C.applicativeEffDictionary) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isFn (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure or return function
  isPurePoly = isFn (C.controlApplicative, C.pure')
  -- Check if an expression represents a function in the Eff module
  isEffFunc name (CppAccessor (CppVar name') (CppVar eff)) = eff == C.eff && name == name'
  isEffFunc _ _ = False

  -- Remove __do function applications which remain after desugaring
  undo :: Cpp -> Cpp
  undo (CppReturn (CppApp (CppFunction ident [] _ _ body) [])) | ident == fnName = body
  undo (CppFunction ident [] rty _ body) | ident == fnName = CppLambda [CppCaptureAll] [] rty body
  undo other = other

  applyReturns :: Cpp -> Cpp
  applyReturns (CppReturn ret) = CppReturn (CppApp ret [])
  applyReturns (CppBlock cpps) = CppBlock (map applyReturns cpps)
  applyReturns (CppWhile cond cpp) = CppWhile cond (applyReturns cpp)
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
   CppLambda [CppCaptureAll] [] Nothing (CppBlock [CppReturn $ if agg then arg else CppObjectLiteral CppRecord [(CppSymbol C.stRefValue, arg)]])
  convert agg (CppApp (CppApp f [ref]) []) | isSTFunc C.readSTRef f =
    if agg then ref else CppAccessor (CppVar C.stRefValue) ref
  convert agg (CppApp (CppApp (CppApp f [ref]) [arg]) []) | isSTFunc C.writeSTRef f =
    if agg then CppAssignment ref arg else CppAssignment (CppAccessor (CppVar C.stRefValue) ref) arg
  convert agg (CppApp (CppApp (CppApp f [ref]) [func]) []) | isSTFunc C.modifySTRef f =
    if agg then CppAssignment ref (CppApp func [ref]) else CppAssignment (CppAccessor (CppVar C.stRefValue) ref) (CppApp func [CppAccessor (CppVar C.stRefValue) ref])
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (CppAccessor (CppVar name') (CppVar st)) = st == C.st && name == name'
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everythingOnCpp (++) isSTRef
    where
    isSTRef (CppVariableIntroduction (ident, _) _ (Just (CppApp (CppApp f [_]) []))) | isSTFunc C.newSTRef f = [ident]
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
