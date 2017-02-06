---------------------------------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Recursion
-- Copyright   :  (c) 2016 Andy Arvanitis
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
-- Stability   :  experimental
-- Portability :
--
--
---------------------------------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp.Recursion
  ( convertRecursiveLets
  ) where

import Prelude.Compat
import Data.List
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)

import Language.PureScript.CodeGen.Cpp.AST as AST
import Language.PureScript.CodeGen.Cpp.File
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Comments (Comment(..))
import qualified Language.PureScript.Pretty.Cpp as P

-------------------------------------------------------------------------------------------------
convertRecursiveLets :: Cpp -> Cpp
-------------------------------------------------------------------------------------------------
convertRecursiveLets = everywhereOnCpp go
  where
  go :: Cpp -> Cpp
  go (CppFunction name args rty qs (CppBlock cpps)) =
    CppFunction name args rty qs (CppBlock (convertIfRecursive cpps))
  go (CppLambda cs args rty (CppBlock cpps)) =
    CppLambda cs args rty (CppBlock (convertIfRecursive cpps))
  go cpp = cpp
  convertIfRecursive :: [Cpp] -> [Cpp]
  convertIfRecursive cpps
    | names@(_:_) <- recNames cpps = convertRecursive names cpps
    where
    recNames :: [Cpp] -> [Text]
    recNames cpps' = nub $ concatMap (everythingOnCpp (++) recRefs) cpps'
      where
      recRefs :: Cpp -> [Text]
      recRefs (CppVariableIntroduction _ qs (Just cpp))
        | Recursive `elem` qs = everythingOnCpp (++) recRef cpp
      recRefs _ = []
      --
      recRef :: Cpp -> [Text]
      recRef (CppVar name') | name' `elem` possiblyRecNames = [name']
      recRef _ = []
      --
      possiblyRecNames :: [Text]
      possiblyRecNames = concatMap (everythingOnCpp (++) possiblyRecName) cpps'
        where
        possiblyRecName :: Cpp -> [Text]
        possiblyRecName (CppVariableIntroduction (name', _) qs (Just _))
          | Recursive `elem` qs = [name']
        possiblyRecName _ = []
  convertIfRecursive cpp = cpp

-------------------------------------------------------------------------------------------------
convertRecursive :: [Text] -> [Cpp] -> [Cpp]
-------------------------------------------------------------------------------------------------
convertRecursive names cpps
  | (_:_) <- lets
  = let recLets = everywhereOnCppTopDown convRecLet <$> cpps
        letDeps = catMaybes (letDep <$> cpps)
    in nub (letDeps ++ recLets) ++ undefines
  where
  tableName :: Text
  tableName = "let"
  --
  letTable :: Cpp
  letTable =
    CppVariableIntroduction
      (tableName, Just $ Any [Const])
      []
      (Just $ CppDataLiteral ((\(f, cpp) -> CppComment [LineComment f] cpp) <$> lets))
  --
  lets :: [(Text, Cpp)]
  lets = tableItem <$> concatMap (everythingOnCpp (++) recLet) cpps
  --
  recLet :: Cpp -> [Cpp]
  recLet cpp@(CppVariableIntroduction (name, _) _ (Just _))
    | name `elem` names = [cpp]
  recLet _ = []
  --
  convRecLet :: Cpp -> Cpp
  convRecLet cpp@(CppVariableIntroduction (name, _) _ (Just _))
    | name == head names = CppNamespace "" $ defines ++ [letTable]
    | name `elem` names = CppNoOp
    | otherwise = cpp
  convRecLet cpp = cpp
  --
  letDep :: Cpp -> Maybe Cpp
  letDep cpp@(CppVariableIntroduction (name, _) _ (Just _))
    | name `elem` names = Nothing
    | everythingOnCpp (||) (\v -> v == CppVar name) letTable = Just cpp
    | otherwise = Nothing
  letDep _ = Nothing
  --
  tableItem :: Cpp -> (Text, Cpp)
  tableItem (CppVariableIntroduction (name, _) _ (Just body)) =
    (name, maybeRemCaps . addItemArg $ CppBlock [CppReturn body])
    where
    addItemArg :: Cpp -> Cpp
    addItemArg cpp = CppLambda [CaptureAll] [(tableName, constAnyRef)] (Just $ Any []) (CppBlock [CppReturn cpp])
  tableItem _ = error "not a variable introduction"
  --
  tableAccessor :: Text -> (Text, Cpp)
  tableAccessor name =
    ( name
    , CppApp
        (CppDataGet
           (CppNumericLiteral . Left . fromIntegral . fromJust $ findIndex ((== name) . fst) lets)
           (CppVar tableName))
      [CppVar tableName])
  --
  defines :: [Cpp]
  defines = def . tableAccessor . fst <$> lets
    where
    def (name, cpp) = CppRaw $ "#define " <> name <> " " <> P.prettyPrintCpp1 cpp <> "#"
  --
  undefines :: [Cpp]
  undefines = undef . tableAccessor . fst <$> lets
    where
    undef (name, _) = CppRaw $ "#undef " <> name <> "#"
  --
convertRecursive _ cpps = cpps
