-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.RankN
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Rank-N type generation utility functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp.RankN where

import Data.Char
import Data.List

import Control.Applicative

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.CoreFn
import Language.PureScript.Names

import qualified Language.PureScript.Types as T

-------------------------------------------------------------------------------------------------
hasRankN :: Maybe T.Type -> Bool
-------------------------------------------------------------------------------------------------
hasRankN Nothing = False
hasRankN (Just (T.ForAll _ t' _)) = hasRankN (Just t')
hasRankN (Just t) = T.everythingOnTypes (||) (not . T.isMonoType) t

---------------------------------------------------------------------------------------------------
replaceRankNs :: ModuleName -> [(Qualified Ident, T.Type)] -> Expr Ann -> Expr Ann
---------------------------------------------------------------------------------------------------
replaceRankNs mn vs | (_, f, _) <- everywhereOnValues id go id = f
  where

  go :: Expr Ann -> Expr Ann
  go e@(Var (_, _, Just t, _) _)
    | T.everythingOnTypes (||) (not . T.isMonoType) t = e
  go e@(Var (_, _, Just t, _) v)
   | Just rankNTy <- lookup (cleanName v) vs,
     Just rankNTyp <- mktype mn rankNTy,
     Just varTyp <- mktype mn t,
     mappings@(_:_) <- templateMappings (rankNTyp, varTyp)
     = App nullAnn e (Var nullAnn (typevals mappings))
  go (App ann@(_, _, Just t, _) f'@(Var _ v) a')
    | Just rankNTy <- lookup (cleanName v) vs,
      Just rankNTyp <- mktype mn rankNTy,
      Just retTyp <- mktype mn t,
      Just argTyp <- typFromExpr a',
      mappings@(_:_) <- templateMappings (rankNTyp, Function argTyp retTyp)
      = App ann (App nullAnn f' (Var nullAnn (typevals mappings))) a'
  go e = e

  cleanName :: Qualified Ident -> Qualified Ident
  cleanName (Qualified m (Ident s))
    | rev@(c:_) <- reverse s,
      isDigit c,
      ('_': ss@(_:_)) <- dropWhile isDigit rev = Qualified m (Ident (reverse ss))
  cleanName n = n

  typFromExpr :: Expr Ann -> Maybe Type
  typFromExpr expr = tyFromExpr expr >>= mktype mn

-- TODO: what about shadowed names?
--
---------------------------------------------------------------------------------------------------
handleRankNCpps :: [Type] -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
handleRankNCpps tmplts cpp = everywhereOnCpp go cpp
  where
  go :: Cpp -> Cpp
  go cpp'@(CppPartialApp _ _ ts _)
    | rns@(_:_) <- (nub $ concatMap templateVars ts) \\ tmplts = rankNWrapper rns cpp'
  go cpp'@(CppApp (CppDataConstructor _ ts) _)
    | rns@(_:_) <- (nub $ concatMap templateVars ts) \\ tmplts = rankNWrapper rns cpp'
  go cpp' = cpp'

---------------------------------------------------------------------------------------------------
rankNWrapper :: [Type] -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
rankNWrapper tmplts cpp = CppLambda []
                                    ((\t -> ('_' : t, Just AutoType)) <$> tmplts')
                                    Nothing
                                    (CppBlock ((typeAlias <$> tmplts') ++ [CppReturn cpp]))
  where
  tmplts' :: [String]
  tmplts' = nub . sort $ runType <$> tmplts
  typeAlias :: String -> Cpp
  typeAlias t = CppTypeAlias (t, []) (runType (DeclType ('_' : t)), []) []

-- TODO: what about shadowed names?
--
---------------------------------------------------------------------------------------------------
getRankNs :: Expr Ann -> [(Qualified Ident, T.Type)]
---------------------------------------------------------------------------------------------------
getRankNs | (_, f, _, _) <- everythingOnValues (++) (const []) go (const []) (const []) = f
  where
  go :: Expr Ann -> [(Qualified Ident, T.Type)]
  go (Var (_, _, Just t, _) v)
    | T.everythingOnTypes (||) (not . T.isMonoType) t = [(v, t)]
  go _ = []
