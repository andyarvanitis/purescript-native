-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.File
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- File generation utility functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp.File where

import Data.Char (isAlphaNum)
import Data.List
import Data.Maybe
import Control.Applicative

import Language.PureScript.CodeGen.Cpp.AST
-- import Language.PureScript.CodeGen.Cpp.Templates
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Names

import qualified Language.PureScript.Pretty.Cpp as P
import qualified Language.PureScript.Pretty.Common as P

---------------------------------------------------------------------------------------------------
toHeader :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toHeader = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps) = Just (CppNamespace name (toHeader cpps))
  go cpp@(CppUseNamespace{}) = Just cpp
  -- go (CppStruct (s, []) ts@(_:_) _ ms@(_:_) [])
  --   | all (not . declHasTemplates) ms = Just (CppStruct (s, []) ts [] [] [])
  -- go (CppStruct (s, []) ts@(_:_) _ ms@(_:_) [])
  --   | ms'@(_:_) <- catMaybes (fromConst <$> ms) = Just (CppSequence ms')
  --   where
  --   fromConst :: Cpp -> Maybe Cpp
  --   fromConst (CppVariableIntroduction (name, typ@(Just _)) tmps qs cpp)
  --     | CppStatic `elem` qs =
  --       Just $ CppVariableIntroduction (fullname name, typ) tmps [CppTemplSpec] cpp
  --   fromConst (CppFunction name tmplts args rtyp qs body) =
  --     Just $ CppFunction (fullname name) tmplts args rtyp (CppTemplSpec : (qs \\ [CppInline, CppStatic])) body
  --   fromConst (CppComment comms cpp) | Just cpp' <- fromConst cpp = Just (CppComment comms cpp')
  --   fromConst _ = Nothing
  --   fullname :: String -> String
  --   fullname name = s ++ '<' : intercalate "," (runType <$> ts) ++ ">::" ++ name
  go (CppStruct (s, []) ts supers ms@(_:_) [])
    | ms'@(_:_) <- fromConst <$> ms = Just (CppStruct (s, []) ts supers ms' [])
    where
    fromConst :: Cpp -> Cpp
    fromConst (CppVariableIntroduction (name, typ@(Just _)) tmps qs _) =
      CppVariableIntroduction (name, typ) tmps qs Nothing
    fromConst cpp = cpp
  go cpp@(CppStruct{}) = Just cpp
  go (CppFunction _ _ [(_, atyp)] _ _ (CppBlock [CppReturn (CppApp _ [_])])) | atyp == Just AutoType
    = Just CppNoOp
  go (CppFunction name tmplts args rtyp qs _) =
    let args' = (\(_,t) -> ("", t)) <$> args in
    Just (CppFunction name tmplts args' rtyp qs CppNoOp)
  go (CppVariableIntroduction v@(name, Just t) [] qs (Just _))
    | t /= AutoType, all isAlphaNum name
    = Just (CppVariableIntroduction v [] (CppExtern:qs) Nothing)
  go cpp@(CppVariableIntroduction{}) = Just cpp
  go (CppComment comms cpp')
    | Just cpp <- go cpp' = Just $ case cpp of CppFunction {} -> cpp
                                               _ -> CppComment comms cpp
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toHeaderFns :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toHeaderFns = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps) = Just (CppNamespace name (toHeaderFns cpps))
  go (CppFunction name tmps [(_, atyp)] _ _ (CppBlock [CppReturn (CppApp cpp [_])]))
    | atyp == Just AutoType
    = Just (CppVariableIntroduction (name, Nothing) tmps [] (Just cpp))
  go cpp@(CppFunction _ (_:_) _ _ _ _) = Just cpp
  go (CppComment comms cpp') | Just cpp <- go cpp' = Just (CppComment comms cpp)
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toBodyDecl :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toBodyDecl = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  -- go (CppStruct (s, []) ts@(_:_) _ ms@(_:_) _)
  --   | all (not . declHasTemplates) ms,
  --     ms'@(_:_) <- catMaybes (fromConst <$> ms) = Just (CppSequence ms')
  --   where
  --   fromConst :: Cpp -> Maybe Cpp
  --   fromConst (CppVariableIntroduction (name, typ@(Just _)) tmps qs _)
  --     | CppStatic `elem` qs =
  --       Just $ CppVariableIntroduction (fullname name, typ) tmps (CppTemplSpec : (delete CppStatic qs)) Nothing
  --   fromConst (CppFunction name tmplts args rtyp qs _) =
  --     Just $ CppFunction (fullname name) tmplts args rtyp (CppTemplSpec : (qs \\ [CppInline, CppStatic])) CppNoOp
  --   fromConst (CppComment _ cpp) = fromConst cpp
  --   fromConst _ = Nothing
  --   fullname :: String -> String
  --   fullname name = s ++ '<' : intercalate "," (runType <$> ts) ++ ">::" ++ name
  go (CppComment comms cpp') | Just commented <- go cpp' = Just (CppComment comms commented)
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toBody :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toBody = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps) =
    let cpps' = toBody cpps in
    if all isNoOp cpps'
      then Nothing
      else Just (CppNamespace name cpps')
    where
    isNoOp :: Cpp -> Bool
    isNoOp CppNoOp = True
    isNoOp (CppComment _ cpp) | isNoOp cpp = True
    isNoOp (CppUseNamespace{}) = True
    isNoOp (CppNamespace _ cpps') = all isNoOp cpps'
    isNoOp (CppRaw _) = True
    isNoOp _ = False
  go cpp@(CppUseNamespace{}) = Just cpp
  go cpp@(CppFunction _ [] _ _ _ _) = Just cpp
  -- go (CppStruct (s, []) ts@(_:_) _ ms@(_:_) _)
  --   | all (not . declHasTemplates) ms,
  --     ms'@(_:_) <- catMaybes (fromConst <$> ms) = Just (CppSequence ms')
  --   where
  --   fromConst :: Cpp -> Maybe Cpp
  --   fromConst (CppVariableIntroduction (name, typ@(Just _)) tmps qs cpp)
  --     | CppStatic `elem` qs =
  --       Just $ CppVariableIntroduction (fullname name, typ) tmps (CppTemplSpec:(delete CppStatic qs)) cpp
  --   fromConst (CppFunction name tmplts args rtyp qs body) =
  --     Just $ CppFunction (fullname name) tmplts args rtyp (CppTemplSpec : (qs \\ [CppInline, CppStatic])) body
  --   fromConst (CppComment comms cpp) | Just cpp' <- fromConst cpp = Just (CppComment comms cpp')
  --   fromConst _ = Nothing
  --   fullname :: String -> String
  --   fullname name = s ++ '<' : intercalate "," (runType <$> ts) ++ ">::" ++ name
  go cpp@(CppVariableIntroduction (name, Just t) [] _ (Just _))
    | t /= AutoType, all isAlphaNum name
    = Just cpp
  go (CppComment comms cpp') | Just commented <- go cpp' = Just (CppComment comms commented)
  go _ = Nothing

---------------------------------------------------------------------------------------------------

fileBegin :: ModuleName -> String -> [Cpp]
fileBegin mn suffix = [CppRaw ("#ifndef " ++ fileModName mn suffix),
                       CppRaw ("#define " ++ fileModName mn suffix)]

fileEnd :: ModuleName -> String -> [Cpp]
fileEnd mn suffix = [CppRaw ("#endif // " ++ fileModName mn suffix)]

fileModName :: ModuleName -> String -> String
fileModName mn suffix = P.dotsTo '_' (runModuleName mn ++ '_' : suffix)

headerDefsBegin :: ModuleName -> [Cpp]
headerDefsBegin mn = [CppRaw ("#if !defined" ++ P.parens (fileModName mn "CC")),
                      CppRaw "#define EXTERN(e) extern e",
                      CppRaw "#else",
                      CppRaw "#define EXTERN(e)",
                      CppRaw "#endif"]

headerDefsEnd :: [Cpp]
headerDefsEnd = [CppRaw "#undef EXTERN"]

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False

nativeMain :: Cpp
nativeMain = CppFunction "main"
               []
               [ ([], Just (Primitive "int"))
               , ([], Just (Primitive "char *[]"))
               ]
               (Just (Native "int" []))
               []
               (CppBlock [ CppApp (CppAccessor Nothing (CppVar "main") (CppScope "Main")) []
                         , CppRaw ";"
                         , CppReturn (CppNumericLiteral (Left 0))
                         ])
