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
  ( convertRecursiveRefs
  ) where

import Prelude.Compat
import Data.List
import Data.Maybe (fromJust)
import Data.Text (Text)

import Language.PureScript.CodeGen.Cpp.AST as AST
import Language.PureScript.CodeGen.Cpp.File
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Comments (Comment(..))

-------------------------------------------------------------------------------------------------
convertRecursiveRefs :: Cpp -> Cpp
-------------------------------------------------------------------------------------------------
convertRecursiveRefs = everywhereOnCpp go
  where
  go :: Cpp -> Cpp
  go (CppFunction name args rty qs (CppBlock cpps)) =
    CppFunction name args rty qs (CppBlock (convertRecursive cpps))
  go (CppLambda cs args rty (CppBlock cpps)) =
    CppLambda cs args rty (CppBlock (convertRecursive cpps))
  go cpp = cpp
  convertRecursive :: [Cpp] -> [Cpp]
  convertRecursive cpps
    | hasRecursion cpps
    = convertRecursiveRefs' cpps
    where
    hasRecursion :: [Cpp] -> Bool
    hasRecursion cpps' = any (everythingOnCpp (||) hasRecursiveRef) cpps'
      where
      valnames :: [Text]
      valnames = concatMap (everythingOnCpp (++) valname) cpps'
      valname :: Cpp -> [Text]
      valname (CppFunction name' _ _ _ _) = [name']
      valname (CppVariableIntroduction (name', _) _ _) = [name']
      valname _ = []
      hasRecursiveRef :: Cpp -> Bool
      hasRecursiveRef (CppFunction _ _ _ _ body) = everythingOnCpp (||) ref body
      hasRecursiveRef (CppVariableIntroduction _ _ (Just value)) = everythingOnCpp (||) ref value
      hasRecursiveRef _ = False
      ref :: Cpp -> Bool
      ref (CppVar name')
        | name' `elem` valnames = True
      ref _ = False
  convertRecursive cpp = cpp

-------------------------------------------------------------------------------------------------
convertRecursiveRefs' :: [Cpp] -> [Cpp]
-------------------------------------------------------------------------------------------------
convertRecursiveRefs' cpps
  | not $ null fns = dict : (accessor topdict <$> filteredFns allCpps) ++ cpps'
  where
  cpps' = everywhereOnCpp removeRec <$> cpps
  allCpps = CppBlock cpps'
  fns :: [(Text, Cpp)]
  fns = toelem <$> concatMap (everythingOnCpp (++) recursive) cpps
  filteredFns :: Cpp -> [(Text, Cpp)]
  filteredFns cpp = filter (\(f, _) -> everythingOnCpp (||) (== CppVar f) cpp) fns
  recursive :: Cpp -> [Cpp]
  recursive cpp'@(CppFunction _ _ _ qs _)
    | Recursive `elem` qs = [cpp']
  recursive cpp'@(CppVariableIntroduction _ qs _)
    | Recursive `elem` qs = [cpp']
  recursive _ = []
  removeRec :: Cpp -> Cpp
  removeRec (CppFunction _ _ _ qs _)
    | Recursive `elem` qs = CppNoOp
  removeRec (CppVariableIntroduction _ qs _)
    | Recursive `elem` qs = CppNoOp
  removeRec cpp' = cpp'
  replaceVar :: [(Text, Cpp)] -> Cpp -> Cpp
  replaceVar rs = go
    where
    go (CppVar var)
      | Just var' <- lookup var rs = var'
    go cpp' = cpp'
  dict :: Cpp
  dict =
    CppVariableIntroduction
      (topdict, Just $ Any [Const])
      []
      (Just $ CppDataLiteral ((\(f, cpp) -> CppComment [LineComment f] cpp) <$> fns))
  accessed :: Text -> Text -> (Text, Cpp)
  accessed dictname name =
    ( name
    , CppApp
        (CppDataGet
           (CppNumericLiteral . Left . fromIntegral . fromJust $ findIndex ((== name) . fst) fns)
           dict')
        [dict'])
    where
    dict' = CppVar dictname
  accessor :: Text -> (Text, Cpp) -> Cpp
  accessor dictname (name, _) =
    CppVariableIntroduction (name, Just $ Any [Const]) [] (Just . snd $ accessed dictname name)
  toelem :: Cpp -> (Text, Cpp)
  toelem (CppFunction name args rtyp _ body'@(CppBlock body)) =
    ( name
    , withdict . maybeRemCaps $
      CppLambda
        [CaptureAll]
        args
        rtyp
        (CppBlock $ (accessor localdict <$> filteredFns body') ++ body))
  toelem (CppVariableIntroduction (name, _) _ (Just body')) =
    let replacements = accessed localdict . fst <$> filteredFns body'
        replaced = everywhereOnCpp (replaceVar replacements) body'
    in (name, withdict . maybeRemCaps $ CppBlock [CppReturn replaced])
  toelem _ = error "not a function"
  withdict :: Cpp -> Cpp
  withdict cpp' =
    maybeRemCaps $
    CppLambda
      [CaptureAll]
      [(localdict, Just $ Any [Const, Ref])]
      (Just $ Any [])
      (CppBlock [CppReturn cpp'])
  topdict :: Text
  topdict = "_$dict$_"
  localdict :: Text
  localdict = "$dict$"
convertRecursiveRefs' cpps = cpps
