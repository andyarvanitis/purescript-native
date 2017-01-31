-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.File
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- File generation utility functions
--
-----------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.CodeGen.Cpp.File where

import Prelude.Compat

import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Common
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Names

---------------------------------------------------------------------------------------------------
toHeader :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toHeader = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps)
    | cpps'@(_:_) <- toHeader cpps = Just (CppNamespace name cpps')
    | otherwise = Nothing
  go cpp@(CppUseNamespace {}) = Just cpp
  go (CppFunction _ _ _ qs _)
    | Inline `elem` qs = Nothing
  go (CppFunction _ [(_, atyp)] _ _ (CppBlock [CppReturn (CppApp _ [_])]))
    | atyp == Just (Auto []) = Nothing
  go (CppFunction name args rtyp qs _) =
    let args' = (\(_, t) -> ("", t)) <$> args
    in Just (CppFunction name args' rtyp qs CppNoOp)
  go (CppVariableIntroduction _ _ (Just CppNumericLiteral {})) = Nothing
  go (CppVariableIntroduction _ _ (Just CppStringLiteral {})) = Nothing
  go (CppVariableIntroduction _ _ (Just CppBooleanLiteral {})) = Nothing
  go (CppVariableIntroduction _ _ (Just (CppUnary _ (CppNumericLiteral {})))) = Nothing
  go (CppVariableIntroduction (name, _) _ (Just (CppLambda _ [] rty _))) =
    Just $ CppFunction name [] rty [] CppNoOp
  -- Generate thunks for top-level values
  go (CppVariableIntroduction (name, _) _ (Just cpp)) =
    case cpp of
      CppDictLiteral _ ->
        Just $ CppFunction name [("", Just thunkMarkerType)] (Just $ Any [Const, Ref]) [] CppNoOp
      _ -> Just $ CppVariableIntroduction (name, Just $ Any [Const]) [Extern] Nothing
  go cpp@(CppStruct {}) = Just cpp
  go (CppComment comms cpp')
    | Just cpp <- go cpp' =
      Just $
      case cpp of
        CppFunction {} -> cpp
        _ -> CppComment comms cpp
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toHeaderFns :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toHeaderFns = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps)
    | cpps'@(_:_) <- toHeaderFns cpps = Just (CppNamespace name cpps')
    | otherwise = Nothing
  go cpp@(CppFunction _ _ _ qs _)
    | Inline `elem` qs = Just cpp
  go (CppFunction name [(_, atyp)] _ _ (CppBlock [CppReturn (CppApp cpp [_])]))
    | Just Auto {} <- atyp = Just (CppVariableIntroduction (name, Nothing) [] (Just cpp))
  go cpp@(CppVariableIntroduction _ _ (Just CppNumericLiteral {})) = Just cpp
  go cpp@(CppVariableIntroduction _ _ (Just CppStringLiteral {})) = Just cpp
  go cpp@(CppVariableIntroduction _ _ (Just CppBooleanLiteral {})) = Just cpp
  go cpp@(CppVariableIntroduction _ _ (Just (CppUnary _ (CppNumericLiteral {})))) = Just cpp
  go (CppComment comms cpp')
    | Just cpp <- go cpp' = Just (CppComment comms cpp)
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toBodyDecl :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toBodyDecl = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppComment comms cpp')
    | Just commented <- go cpp' = Just (CppComment comms commented)
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toBody :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toBody = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps)
    | cpps'@(_:_) <- toBody cpps = Just (CppNamespace name cpps')
    | otherwise = Nothing
  go cpp@(CppUseNamespace {}) = Just cpp
  go (CppFunction _ _ _ qs _)
    | Inline `elem` qs = Nothing
  go cpp@(CppFunction {}) = Just cpp
  go (CppVariableIntroduction _ _ (Just CppNumericLiteral {})) = Nothing
  go (CppVariableIntroduction _ _ (Just CppStringLiteral {})) = Nothing
  go (CppVariableIntroduction _ _ (Just CppBooleanLiteral {})) = Nothing
  go (CppVariableIntroduction _ _ (Just (CppUnary _ (CppNumericLiteral {})))) = Nothing
  go (CppVariableIntroduction (name, _) _ (Just (CppLambda _ [] rty body))) =
    Just $ CppFunction name [] rty [] body
  -- Generate thunks for top-level values
  go (CppVariableIntroduction (name, _) _ (Just cpp)) =
    case cpp of
      CppDictLiteral _ ->
        Just $ CppFunction name [("", Just thunkMarkerType)] (Just $ Any [Const, Ref]) [] block
      _ -> Just $ CppVariableIntroduction (name, Just $ Any [Const]) [] (Just lambda)
    where
    val = CppVariableIntroduction ("the_value", Just $ Any [Const]) [Static] (Just $ addCaptures cpp)
    block = CppBlock [val, CppReturn (CppVar "the_value")]
    lambda = CppLambda [] [("", Just $ thunkMarkerType)] (Just $ Any [Const, Ref]) block
    addCaptures :: Cpp -> Cpp
    addCaptures (CppDictLiteral objs) = CppDictLiteral (objlam <$> objs)
      where
      objlam :: (Cpp, Cpp) -> (Cpp, Cpp)
      objlam (name', (CppLambda _ args rty body)) =
        (name', CppLambda [] args rty $ addCaptures body)
      objlam obj = obj
    addCaptures (CppApp (CppLambda _ [] rty body) []) =
      CppApp (CppLambda [] [] rty (addCaptures body)) []
    addCaptures cpps' = everywhereOnCpp addCapture cpps'
      where
      addCapture :: Cpp -> Cpp
      addCapture (CppLambda _ args rty body) = maybeRemCaps $ CppLambda [CaptureAll] args rty body
      addCapture cpp' = cpp'
  go (CppComment comms cpp')
    | Just commented <- go cpp' = Just (CppComment comms commented)
  go _ = Nothing

-------------------------------------------------------------------------------------------------
maybeRemCaps :: Cpp -> Cpp
-------------------------------------------------------------------------------------------------
maybeRemCaps (CppLambda [CaptureAll] args rtyp body@(CppBlock [CppReturn CppNumericLiteral {}])) =
  CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CaptureAll] args rtyp body@(CppBlock [CppReturn CppStringLiteral {}])) =
  CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CaptureAll] args rtyp body@(CppBlock [CppReturn CppCharLiteral {}])) =
  CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CaptureAll] args rtyp body@(CppBlock [CppReturn CppBooleanLiteral {}])) =
  CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CaptureAll] args rtyp body@(CppBlock [CppReturn CppAccessor {}])) =
  CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CaptureAll] args rtyp body@(CppBlock [CppReturn (CppVar "unit")])) -- TODO: not really safe
 = CppLambda [] args rtyp body
maybeRemCaps cpp = cpp

---------------------------------------------------------------------------------------------------
fileBegin :: ModuleName -> Text -> [Cpp]
fileBegin mn suffix =
  [CppRaw ("#ifndef " <> fileModName mn suffix), CppRaw ("#define " <> fileModName mn suffix)]

fileEnd :: ModuleName -> Text -> [Cpp]
fileEnd mn suffix = [CppRaw ("#endif // " <> fileModName mn suffix)]

fileModName :: ModuleName -> Text -> Text
fileModName mn suffix = dotsTo '_' (runModuleName mn <> "_" <> suffix)

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False

nativeMain :: Cpp
nativeMain =
  CppFunction
    "main"
    [("", Just $ Primitive "int"), ("", Just $ Primitive "char *[]")]
    (Just $ Primitive "int")
    []
    (CppBlock
       [ CppUseNamespace "Main"
       , CppApp (CppVar "INITIALIZE_GC") []
       , CppApp (CppAccessor (CppVar "main") (CppVar "Main")) []
       , CppReturn (CppNumericLiteral (Left 0))
       ])
