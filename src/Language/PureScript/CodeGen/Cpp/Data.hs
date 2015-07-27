-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Data
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Data generation utility functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp.Data where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Control.Applicative
-- import Control.Monad.Reader (MonadReader, asks)
-- import Control.Monad.Supply.Class
-- import Control.Monad (when, liftM2)

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Templates
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Names

import qualified Language.PureScript.Environment as E
import qualified Language.PureScript.Pretty.Cpp as P
import qualified Language.PureScript.Types as T

---------------------------------------------------------------------------------------------------
datasToCpps :: Monad m => E.Environment -> ModuleName -> m [Cpp]
---------------------------------------------------------------------------------------------------
datasToCpps env mn
  | ds@(_:_) <- M.toList
               . M.mapWithKey (\_ a -> snd a)
               . M.filterWithKey (\(Qualified mn' _) _ -> mn' == Just mn)
               . M.filter (isData . snd)
               $ E.types env = do
    let types' = dataTypes ds
        (ctors, aliases) = dataValueCtors ds
    return (types' ++ asManaged types' ++ ctors ++ asManaged ctors ++ aliases)
  | otherwise = return []
    where
    isData :: E.TypeKind -> Bool
    isData E.DataType{} = True
    isData _ = False

    asManaged:: [Cpp] -> [Cpp]
    asManaged = concatMap go
      where
      go :: Cpp -> [Cpp]
      go (CppNamespace ns cpps) = fromStruct ns <$> cpps
      go _ = []
      -- TODO: this feels too fragile
      fromStruct :: String -> Cpp -> Cpp
      fromStruct _ (CppStruct (_, (_:_)) [] [] [] []) = CppNoOp
      fromStruct _ (CppStruct _ _ _ _ [CppTypeAlias{}]) = CppNoOp
      fromStruct ns (CppStruct (name, tmplts) _ _ _ _) =
        let tmplts' = remTemplateDefaults tmplts in
        CppTypeAlias (name, tmplts') (ns ++ "::" ++ name, tmplts') "managed"
      fromStruct _ _ = CppNoOp

    dataTypes :: [(Qualified ProperName, E.TypeKind)] -> [Cpp]
    dataTypes ds | cpps@(_:_) <- concatMap go ds = [CppNamespace "type" cpps]
                 | otherwise = []
      where
      go :: (Qualified ProperName, E.TypeKind) -> [Cpp]
      -- go (_, E.DataType _ [_]) = []
      go (typ, E.DataType ts cs) =
        let tmplts = tmpltsReplFromRight
                       (flip (,) 0 . runType . mkTemplate . fst <$> ts)
                       (concatMap templateParams (snd <$> cs))
        in dataTypeCtors (qual typ) tmplts
      go _ = []

    dataTypeCtors :: String -> [TemplateInfo] -> [Cpp]
    dataTypeCtors name [] =
      [CppStruct (name, []) [] [] []
        [CppFunction name [] [] Nothing [CppVirtual, CppDestructor, CppDefault] CppNoOp]]
    dataTypeCtors name [param] = [
        CppStruct (name, [param])
                  [] [] []
                  [CppFunction name [] [] Nothing [CppVirtual, CppDestructor, CppDefault] CppNoOp]
      ]
    dataTypeCtors name params@(p:ps) = [
        -- The template declaration
        CppStruct (name, addTemplateDefaults params) [] [] [] []
        -- The fully applied type constructor
      , CppStruct (name, params)
                  [] [] []
                  [CppFunction name [] [] Nothing [CppVirtual, CppDestructor, CppDefault] CppNoOp]
      ] ++ dataTypePartialCtors name [p] ps

    dataTypePartialCtors :: String -> [TemplateInfo] -> [TemplateInfo] -> [Cpp]
    dataTypePartialCtors name applied notApplied@(p:ps) =
        (CppStruct (name, applied)
                   (typeFromTemplate <$> applied)
                   [] []
                   [CppTypeAlias ("_", notApplied) (name, applied ++ notApplied) []])
      : dataTypePartialCtors name (applied ++ [p]) ps
      where
      typeFromTemplate :: TemplateInfo -> Type
      typeFromTemplate (name', _) = Template name' []
    dataTypePartialCtors _ _ _ = []

    dataValueCtors :: [(Qualified ProperName, E.TypeKind)] -> ([Cpp], [Cpp])
    dataValueCtors ds | cpps@(_:_) <- concatMap (fst . go) ds,
                   aliases <- catMaybes $ map (snd . go) ds = ([CppNamespace "value" cpps], aliases)
                 | otherwise = ([],[])
      where
      go :: (Qualified ProperName, E.TypeKind) -> ([Cpp], Maybe Cpp)
      go (typ, E.DataType ts cs) = (map ctorStruct cs, alias)
        where
        alias :: Maybe Cpp
        alias = Nothing
        tmplts :: [TemplateInfo]
        tmplts = tmpltsReplFromRight
                   (flip (,) 0 . runType . mkTemplate . fst <$> ts)
                   (concatMap templateParams (snd <$> cs))
        ctorStruct :: (ProperName, [T.Type]) -> Cpp
        ctorStruct (ctor, fields) =
          CppStruct (name, tmplts) [] supers' [] members
          where
          name :: String
          name = P.prettyPrintCpp [flip CppData [] $ runProperName ctor]
          supers' :: [(String, [String])]
          supers'
            | null fieldtypes = supers
            | otherwise = supers ++ [("tuple", fieldtypes)]
          supers :: [(String, [String])]
          supers = [(addNamespace "type" (qual typ), fst <$> tmplts)]
          fieldtypes :: [String]
          fieldtypes =  ("const " ++) . runType <$> (filter (/= Map []) . catMaybes $ mktype mn <$> fields)
          members :: [Cpp]
          members
            | null fieldtypes = []
            | otherwise = [CppVar $ "using tuple<" ++ intercalate ", " fieldtypes ++ ">::tuple;"]
      go _ = ([], Nothing)

    templateParams :: [T.Type] -> [TemplateInfo]
    templateParams = nub . concatMap (templparams' . mktype mn)

    qual :: Qualified ProperName -> String
    qual name = qualifiedToStr mn (Ident . runProperName) name

    addNamespace :: String -> String -> String
    addNamespace ns s | ':' `elem` s,
                        (s1,s2) <- splitAt (last $ findIndices (==':') s) s = s1 ++ ':' : ns ++ ':' : s2
                      | otherwise = ns ++ "::" ++ s
