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

import Data.Maybe

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
  go (CppFunction _ [(_, atyp)] _ _ (CppBlock [CppReturn (CppApp _ [_])])) | atyp == Just AutoType
    = Just CppNoOp
  go (CppFunction name args rtyp qs _) =
    let args' = (\(_,t) -> ("", t)) <$> args in
    Just (CppFunction name args' rtyp qs CppNoOp)
  -- go (CppVariableIntroduction v@(name, Just t) qs (Just _))
  --   | t /= AutoType, all isAlphaNum name
  --   = Just (CppVariableIntroduction v (CppExtern:qs) Nothing)
  -- go cpp@(CppVariableIntroduction{}) = Just cpp

  go (CppVariableIntroduction _ _ (Just CppNumericLiteral {})) =
    Nothing
  go (CppVariableIntroduction _ _ (Just CppStringLiteral {})) =
    Nothing
  go (CppVariableIntroduction _ _ (Just CppBooleanLiteral {})) =
    Nothing
  -- Generate thunks for top-level values
  go (CppVariableIntroduction (name, _) _ (Just cpp)) =
    Just $ CppVariableIntroduction (name, Just AnyType) [CppExtern] Nothing
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
  go (CppFunction name [(_, atyp)] _ _ (CppBlock [CppReturn (CppApp cpp [_])]))
    | atyp == Just AutoType
    = Just (CppVariableIntroduction (name, Nothing) [] (Just cpp))

  go cpp@(CppVariableIntroduction v qs (Just CppNumericLiteral {})) =
    Just cpp
  go cpp@(CppVariableIntroduction v qs (Just CppStringLiteral {})) =
    Just cpp
  go cpp@(CppVariableIntroduction v qs (Just CppBooleanLiteral {})) =
    Just cpp

  go (CppComment comms cpp') | Just cpp <- go cpp' = Just (CppComment comms cpp)
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toBodyDecl :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toBodyDecl = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
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
  go cpp@(CppFunction {}) = Just cpp
  go (CppVariableIntroduction _ _ (Just CppNumericLiteral {})) =
    Nothing
  go (CppVariableIntroduction _ _ (Just CppStringLiteral {})) =
    Nothing
  go (CppVariableIntroduction _ _ (Just CppBooleanLiteral {})) =
    Nothing
  -- Generate thunks for top-level values
  go (CppVariableIntroduction (name, _) _ (Just cpp)) =
    Just $ CppVariableIntroduction (name, Just AnyType) [] (Just lambda)
    where
    val = CppVariableIntroduction ("_value_", Just AnyType) [CppStatic] (Just cpp)
    block = CppBlock [val, CppReturn (CppVar "_value_")]
    lambda = CppLambda [] [("", Just (Native "as_thunk" []))] (Just AnyTypeRef) block

  -- go cpp@(CppVariableIntroduction (name, Just t) [] _ (Just _))
  --   | t /= AutoType, all isAlphaNum name
  --   = Just cpp

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
               [ ([], Just (Primitive "int"))
               , ([], Just (Primitive "char *[]"))
               ]
               (Just (Native "int" []))
               []
               (CppBlock [ CppApp (CppApp (CppAccessor (CppVar "main") (CppVar "Main"))
                                          [CppVar "PureScript::unthunk"])
                                  []
                         , CppRaw ";"
                         , CppReturn (CppNumericLiteral (Left 0))
                         ])
