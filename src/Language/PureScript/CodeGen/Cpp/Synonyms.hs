-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Synonyms
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Type synonym generation utility functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp.Synonyms where

import Data.List
import Data.Maybe
import Data.Tuple (swap)

import qualified Data.Map as M

import Control.Applicative
import qualified Data.Graph as G

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Names

import qualified Language.PureScript.Environment as E

-------------------------------------------------------------------------------------------------
synonymsToCpp :: Monad m => E.Environment -> ModuleName -> m [Cpp]
-------------------------------------------------------------------------------------------------
synonymsToCpp env mn
  | tcs <- E.typeClasses env,
    ds@(_:_) <-  M.toList
               . M.filterWithKey (\k@(Qualified mn' _) _ -> mn' == Just mn && M.lookup k tcs == Nothing)
               $ E.typeSynonyms env = do
    let names' = qualifiedToStr mn (Ident . runProperName) . fst <$> ds
        tmplts = map templateFromKind . fst . snd <$> ds
        typs = catMaybes $ mktype mn . snd . snd <$> ds
        syns = zip3 names' tmplts typs
        (synonyms, invalidSynonyms) = partition isValid syns
        cpps = (\(n,tmps,t) -> CppTypeAlias (n, tmps) t []) <$> synonyms
        rejected = (\(n, _, _) -> CppRaw ("// using " ++ n ++ " = auto;")) <$> invalidSynonyms
    return $ cpps ++ rejected
  | otherwise = return []
  where
  isValid :: (String, a, Type) -> Bool
  isValid (_, _, t) = everythingOnTypes (&&) (/= AutoType) t

-- |
-- Dependency (topological) sorting of synonyms and data types
--
depSortSynonymsAndData :: [Cpp] -> [Cpp]
depSortSynonymsAndData allCpps = consolidateNamespaces . reverse $
  catMaybes $ flip lookup vertexCpps <$> G.topSort (G.buildG (1, length cpps) (concatMap findEdges cpps))
  where
  cpps = filter (/= CppNoOp) allCpps
  findEdges :: Cpp -> [G.Edge]
  findEdges cpp@(CppTypeAlias _ typ _)
    | Just thisVertex <- lookup cpp vertexCpps' = everythingOnTypes (++) (go thisVertex) typ
  findEdges cpp@(CppStruct _ _ supers _ _)
    | Just thisVertex <- lookup cpp vertexCpps',
      typs@(_:_) <- concatMap snd supers
      = concatMap (everythingOnTypes (++) (go thisVertex)) typs
  findEdges _ = []

  go :: G.Vertex -> Type -> [G.Edge]
  go thisVertex (Native name _)
    | Just depVertex <- lookup name vertexes = [(thisVertex, depVertex)]
  go _ _ = []

  vertexes :: [(String, G.Vertex)]
  vertexes = zip (getName <$> cpps) [1 ..]

  vertexCpps :: [(G.Vertex, Cpp)]
  vertexCpps = zip [1 ..] cpps

  vertexCpps' :: [(Cpp, G.Vertex)]
  vertexCpps' = swap <$> vertexCpps

  getName :: Cpp -> String
  getName (CppTypeAlias (name, _) _ _) = name
  getName (CppStruct (name, _) _ _ _ _) = name
  getName (CppNamespace ns [cpp]) = ns ++ "::" ++ getName cpp
  getName cpp = error $ "Wrong kind of Cpp value! " ++ show cpp

consolidateNamespaces :: [Cpp] -> [Cpp]
consolidateNamespaces cpps = foldl sameNamespace [] cpps
  where
  sameNamespace :: [Cpp] -> Cpp -> [Cpp]
  sameNamespace acc@(_:_) (CppNamespace n' bs)
    | CppNamespace n as <- last acc, n == n' = (init acc) ++ [CppNamespace n (as ++ bs)]
  sameNamespace as b = as ++ [b]
