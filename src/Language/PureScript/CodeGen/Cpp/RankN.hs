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

import qualified Language.PureScript.Environment as E
import qualified Language.PureScript.Types as T

-------------------------------------------------------------------------------------------------
tyHasRankNs :: Maybe T.Type -> Bool
-------------------------------------------------------------------------------------------------
tyHasRankNs Nothing = False
tyHasRankNs (Just (T.ForAll _ t' _)) = tyHasRankNs (Just t')
tyHasRankNs (Just t) = T.everythingOnTypes (||) (not . T.isMonoType) t

---------------------------------------------------------------------------------------------------
replaceRankNVals :: ModuleName -> [(Qualified Ident, T.Type)] -> Expr Ann -> Expr Ann
---------------------------------------------------------------------------------------------------
replaceRankNVals mn vs | (_, f, _) <- everywhereOnValues id go id = f
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
wrapRankNCpps :: [Type] -> Cpp -> Cpp
---------------------------------------------------------------------------------------------------
wrapRankNCpps tmplts cpp = everywhereOnCpp go cpp
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
  typeAlias t = CppTypeAlias (t, []) (removeConst . runType $ DeclType ('_' : t), []) []
  removeConst :: String -> String
  removeConst t = "typename remove_const<" ++ t ++ ">::type"

-- TODO: what about shadowed names?
--
---------------------------------------------------------------------------------------------------
getRankNVals :: Expr Ann -> [(Qualified Ident, T.Type)]
---------------------------------------------------------------------------------------------------
getRankNVals | (_, f, _, _) <- everythingOnValues (++) (const []) go (const []) (const []) = f
  where
  go :: Expr Ann -> [(Qualified Ident, T.Type)]
  go (Var (_, _, Just t, _) v)
    | T.everythingOnTypes (||) (not . T.isMonoType) t = [(v, t)]
  go (Abs (_, _, Just t, _) v _)
    | T.everythingOnTypes (||) (not . T.isMonoType) t = [(Qualified Nothing v, argty t)]
    where
    argty (T.TypeApp (T.TypeApp ctor@(T.TypeConstructor _) a) b) | ctor == E.tyFunction = a
    argty a = a
  go _ = []
