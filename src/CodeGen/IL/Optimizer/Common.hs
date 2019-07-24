-- | Common functions used by the various optimizer phases
module CodeGen.IL.Optimizer.Common where

import Prelude.Compat

import Data.Text (Text)

import Language.PureScript.CoreImp.AST
import Language.PureScript.PSString (PSString, decodeString)

import CodeGen.IL.Common (properToIL)

isDict :: (Text, PSString) -> AST -> Bool
isDict (moduleName, dictName) (Indexer _ (Var _ x) (Var _ y)) =
  Just x == (properToIL <$> decodeString dictName) && y == moduleName
isDict _ _ = False

isDict' :: [(Text, PSString)] -> AST -> Bool
isDict' xs il = any (`isDict` il) xs

