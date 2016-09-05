-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.Uncurry
-- Copyright   :  (c) Andy Arvanitis 2016
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Removes unused variables
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Cpp.Optimizer.Uncurry where

import Prelude.Compat

import Language.PureScript.CodeGen.Cpp.AST

type NamesMap = [(Cpp, Int)]

removeCurrying :: NamesMap -> Cpp -> Cpp
removeCurrying nm = everywhereOnCpp convert
  where
  convert cpp@(CppApp {})
    | (f, args@(_:_)) <- unApp cpp [],
      Just f' <- unCurried f,
      Just arity' <- lookup f' nm,
      arity' > 0 && length args >= arity' =
      foldl
        (\fn a -> CppApp fn [a])
        (CppApp f' (take arity' args))
        (drop arity' args)
    where
    unCurried :: Cpp -> Maybe Cpp
    unCurried (CppAccessor var mn)
      | Just var' <- unCurried var = Just $ CppAccessor var' mn
    unCurried (CppVar ('$':name)) = Just $ CppVar name
    unCurried _ = Nothing
  convert cpp = cpp

-------------------------------------------------------------------------------------------------
unApp :: Cpp -> [Cpp] -> (Cpp, [Cpp])
-------------------------------------------------------------------------------------------------
unApp (CppApp val args) args' = unApp val (args ++ args')
unApp other args' = (other, args')
