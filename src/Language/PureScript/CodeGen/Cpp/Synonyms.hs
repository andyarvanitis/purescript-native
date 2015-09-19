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
{-# LANGUAGE CPP #-}

module Language.PureScript.CodeGen.Cpp.Synonyms where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.List
import Data.Maybe
import Data.Tuple (swap)

import qualified Data.Map as M
import qualified Data.Graph as G

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Templates
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
        -- tmplts = map templateFromKind . fst . snd <$> ds
        -- typs = catMaybes $ mktype mn . snd . snd <$> ds
        typs = replicate (length ds) AnyType
        syns = zip names' typs
        (synonyms, invalidSynonyms) = partition isValid syns
        cpps = toTypeAlias <$> synonyms
        rejected = (\(n, _) -> CppRaw ("// using " ++ n ++ " = auto;")) <$> invalidSynonyms
    return $ cpps ++ rejected
  | otherwise = return []
  where
  isValid :: (String, Type) -> Bool
  isValid (_, t) = everythingOnTypes (&&) (/= AutoType) t
  -- toTypeAlias :: (String, [TemplateInfo], Type) -> Cpp
  toTypeAlias :: (String, Type) -> Cpp
  -- toTypeAlias (n, tmps, Template t _) =
  --   CppTypeAlias (n, tmps ++ ptmps) (Template t (templateToType <$> ptmps)) []
  --   where
  --   ptmps :: [TemplateInfo]
  --   ptmps = concatMap go tmps
  --     where
  --     go :: TemplateInfo -> [TemplateInfo]
  --     go (t', n') = (\p -> (t' ++ show p, 0)) <$> [1 .. n']
  -- toTypeAlias (n, tmps, t) = CppTypeAlias (n, tmps) t []
  toTypeAlias (n, t) = CppTypeAlias (n, []) t []

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
