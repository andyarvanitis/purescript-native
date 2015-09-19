-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Templates
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Template generation utility functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.CodeGen.Cpp.Templates where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.List
import Data.Maybe

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Types

isParameterized :: TemplateInfo -> Bool
isParameterized (_, n) | n > 0 = True
isParameterized _ = False

declHasTemplates :: Cpp -> Bool
declHasTemplates = everythingOnCpp (||) go
  where
  go :: Cpp -> Bool
  go (CppFunction _ (_:_) _ _ _ _) = True
  go (CppVariableIntroduction _ (_:_) _ _) = True
  go _ = False

-- TODO: add type/template info to CppVar?
valueHasTemplates :: Cpp -> Bool
valueHasTemplates = everythingOnCpp (||) go
  where
  go :: Cpp -> Bool
  go (CppAccessor (Just t) _ _) | everythingOnTypes (||) isTemplate t = True
  go (CppData _ ts) | any (everythingOnTypes (||) isTemplate) ts = True
  go (CppCast t _) | everythingOnTypes (||) isTemplate t = True
  go (CppPartialApp ts _ _ _) | any (everythingOnTypes (||) isTemplate) ts = True
  go (CppInstance _ _ _ ps) | any (everythingOnTypes (||) isTemplate) $ catMaybes (snd <$> ps) = True
  go (CppVar s) | '<' `elem` s = True
  go _ = False

tmpltsReplFromRight :: [TemplateInfo] -> [TemplateInfo] -> [TemplateInfo]
tmpltsReplFromRight t1s t2s = nub $ map go' t1s
  where
  go' :: TemplateInfo -> TemplateInfo
  go' t1 | Just n <- lookup (fst t1) t2s = (fst t1, n)
         | otherwise = t1

-- TODO: add type/template info to CppVar?
asTemplate :: [Type] -> Cpp -> Cpp
asTemplate [] cpp = cpp
asTemplate ps (CppVar name) = CppVar (name ++ '<' : intercalate "," (runType <$> ps) ++ ">")
asTemplate ps (CppAccessor t prop cpp)
  | Just (Native t' _) <- t = CppAccessor (Just (Native t' ps)) prop cpp
  | otherwise = CppAccessor (Just (Native [] ps)) prop cpp
asTemplate _ cpp = cpp

removeTemplates :: [TemplateInfo] -> Cpp -> Cpp
removeTemplates tmplts = everywhereOnCpp remove
  where
  remove (CppFunction name ts args rtyp qs body) =
    let ts' = filter (`notElem` tmplts) ts in CppFunction name ts' args rtyp qs body
  remove (CppVariableIntroduction name ts qs val) =
    let ts' = filter (`notElem` tmplts) ts in CppVariableIntroduction name ts' qs val
  remove (CppComment comms cpp') = CppComment comms (remove cpp')
  remove other = other

mkTemplate :: String -> Type
mkTemplate s = Template s []

templateToType :: TemplateInfo -> Type
templateToType (t, n) = Template t (replicate n (Template "t" []))
