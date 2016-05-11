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

module Language.PureScript.CodeGen.Cpp.Optimizer.MagicDo (
  magicDo
) where

import Prelude.Compat

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Names
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C

magicDo :: Options -> Cpp -> Cpp
magicDo opts | optionsNoMagicDo opts = id
             | otherwise = magicDo'

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
  convert (CppApp (CppApp ret [effDict, val]) []) | isRetPoly ret && isEffDict C.monadEffDictionary effDict = val
  convert (CppApp (CppApp ret [val]) []) | isReturn ret = val
  -- Desugar pure
  convert (CppApp (CppApp pure' [effDict, val]) []) | isPurePoly pure' && isEffDict C.applicativeEffDictionary effDict = val
  convert (CppApp (CppApp pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  convert (CppApp bind [effDict, m, CppLambda _ [] rty (CppBlock cpp)]) | isBindPoly bind && isEffDict C.bindEffDictionary effDict =
    CppFunction fnName [] rty [] $ CppBlock (CppApp m [] : map applyReturns cpp )
  convert (CppApp (CppApp bind [m]) [CppLambda _ [] rty (CppBlock cpp)]) | isBind bind =
    CppFunction fnName [] rty [] $ CppBlock (CppApp m [] : map applyReturns cpp )
  -- Desugar >>=
  convert (CppApp bind [effDict, m, CppLambda _ [arg] rty (CppBlock cpp)]) | isBindPoly bind && isEffDict C.bindEffDictionary effDict =
    CppFunction fnName [] rty [] $ CppBlock (CppVariableIntroduction arg [] (Just (CppApp m [])) : map applyReturns cpp)
  convert (CppApp (CppApp bind [m]) [CppLambda _ [arg] rty (CppBlock cpp)]) | isBind bind =
    CppFunction fnName [] rty [] $ CppBlock (CppVariableIntroduction arg [] (Just (CppApp m [])) : map applyReturns cpp)
  -- Desugar untilE
  convert (CppApp (CppApp f [arg]) []) | isEffFunc C.untilE f =
    CppApp (CppLambda [CppCaptureAll] [] (Just $ CppAny []) (CppBlock [ CppWhile (CppUnary CppNot (CppApp arg [])) (CppBlock []), CppReturn $ CppObjectLiteral []])) []
  -- Desugar whileE
  convert (CppApp (CppApp f [arg1, arg2]) []) | isEffFunc C.whileE f =
    CppApp (CppLambda [CppCaptureAll] [] (Just $ CppAny []) (CppBlock [ CppWhile (CppApp arg1 []) (CppBlock [ CppApp arg2 [] ]), CppReturn $ CppObjectLiteral []])) []
  convert (CppApp (CppApp (CppApp f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    CppApp (CppLambda [CppCaptureAll] [] (Just $ CppAny []) (CppBlock [ CppWhile (CppApp arg1 []) (CppBlock [ CppApp arg2 [] ]), CppReturn $ CppObjectLiteral []])) []
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
  isBindPoly (CppAccessor (CppVar prop) (CppVar prelude)) = prelude == C.prelude && (prop `elem` map identToCpp [Ident C.bind, Op (C.>>=)])
  isBindPoly (CppIndexer (CppStringLiteral bind) (CppVar prelude)) = prelude == C.prelude && (bind `elem` [C.bind, (C.>>=)])
  isBindPoly _ = False
  -- Check if an expression represents the polymorphic return function
  isRetPoly (CppAccessor (CppVar returnEscaped) (CppVar prelude)) = prelude == C.prelude && returnEscaped == C.returnEscaped
  isRetPoly (CppIndexer (CppStringLiteral return') (CppVar prelude)) = prelude == C.prelude && return' == C.return
  isRetPoly _ = False
  -- Check if an expression represents the polymorphic pure function
  isPurePoly (CppAccessor (CppVar pure') (CppVar prelude)) = prelude == C.prelude && pure' == C.pure'
  isPurePoly (CppIndexer (CppStringLiteral pure') (CppVar prelude)) = prelude == C.prelude && pure' == C.pure'
  isPurePoly _ = False
  -- Check if an expression represents a function in the Ef module
  isEffFunc name (CppAccessor (CppVar name') (CppVar eff)) = eff == C.eff && name == name'
  isEffFunc _ _ = False
  -- Check if an expression represents the Monad Eff dictionary
  isEffDict name (CppVar ident) | ident == name = True
  isEffDict name (CppAccessor (CppVar prop) (CppVar eff)) = eff == C.eff && prop == name
  isEffDict _ _ = False
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
-- TODO: various issues in C++ backend -- revisit later
--
