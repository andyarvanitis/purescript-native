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
        synonyms = zip3 names' tmplts typs
        synonyms' = depSortSynonyms synonyms
        cpps = (\(n,tmps,t) -> CppTypeAlias (n, tmps) (runType t, []) []) <$> synonyms'
    return cpps
  | otherwise = return []
  where
  -- |
  -- Dependency (topological) sorting of synonyms
  --
  depSortSynonyms :: [(String, [(String,Int)], Type)] ->  [(String, [(String,Int)], Type)]
  depSortSynonyms syns = reverse $
    catMaybes $ flip lookup vertexSyns <$> G.topSort (G.buildG (1, length syns) (concatMap findEdges syns))
    where
    findEdges ::  (String, [(String,Int)], Type) -> [G.Edge]
    findEdges syn@(_,_,typ) = everythingOnTypes (++) go typ
      where
      go ::  Type -> [G.Edge]
      go (Native name _)
        | Just thisVertex <- lookup syn vertexSyns',
          Just depVertex <- lookup name vertexes = [(thisVertex, depVertex)]
      go _ = []

    vertexes :: [(String, G.Vertex)]
    vertexes = zip ((\(name,_,_) -> name) <$> syns) [1 ..]

    vertexSyns :: [(G.Vertex, (String, [(String,Int)], Type))]
    vertexSyns = zip [1 ..] syns

    vertexSyns' :: [((String, [(String,Int)], Type), G.Vertex)]
    vertexSyns' = swap <$> vertexSyns
