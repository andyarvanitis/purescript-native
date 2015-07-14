-----------------------------------------------------------------------------
--
-- Module      :  Make
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Make
  ( Make(..)
  , runMake
  , buildMakeActions
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Traversable (traverse)
import Data.Version (showVersion)
import qualified Data.Map as M

import System.Directory
       (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (tryIOError)

import qualified Language.PureScript as P
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Core as CR
import qualified Language.PureScript.CoreImp as CI
import qualified Paths_purescript as Paths

newtype Make a = Make { unMake :: ReaderT (P.Options P.Make) (WriterT P.MultipleErrors (ExceptT P.MultipleErrors IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError P.MultipleErrors, MonadWriter P.MultipleErrors, MonadReader (P.Options P.Make))

runMake :: P.Options P.Make -> Make a -> IO (Either P.MultipleErrors (a, P.MultipleErrors))
runMake opts = runExceptT . runWriterT . flip runReaderT opts . unMake

makeIO :: (IOError -> P.ErrorMessage) -> IO a -> Make a
makeIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . P.singleError . f) return e

-- Traverse (Either e) instance (base 4.7)
traverseEither :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left x) = pure (Left x)
traverseEither f (Right y) = Right <$> f y

buildMakeActions :: FilePath
                 -> M.Map P.ModuleName (Either P.RebuildPolicy String)
                 -> M.Map P.ModuleName (FilePath, P.ForeignJS)
                 -> Bool
                 -> P.MakeActions Make
buildMakeActions outputDir filePathMap foreigns usePrefix =
  P.MakeActions getInputTimestamp getOutputTimestamp readExterns codegen progress
  where

  getInputTimestamp :: P.ModuleName -> Make (Either P.RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn = do
    let path = fromMaybe (error "Module has no filename in 'make'") $ M.lookup mn filePathMap
    e1 <- traverseEither getTimestamp path
    fPath <- maybe (return Nothing) (getTimestamp . fst) $ M.lookup mn foreigns
    return $ fmap (max fPath) e1

  getOutputTimestamp :: P.ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = P.runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
    min <$> getTimestamp jsFile <*> getTimestamp externsFile

  readExterns :: P.ModuleName -> Make (FilePath, String)
  readExterns mn = do
    let path = outputDir </> P.runModuleName mn </> "externs.purs"
    (path, ) <$> readTextFile path

  codegen :: CR.Module (CF.Bind CR.Ann) -> P.Environment -> P.SupplyVar -> P.Externs -> Make ()
  codegen m _ nextVar exts = do
    let mn = CR.moduleName m
    foreignInclude <- case mn `M.lookup` foreigns of
      Just (path, _)
        | not $ requiresForeign m -> do
            tell $ P.errorMessage $ P.UnnecessaryFFIModule mn path
            return Nothing
        | otherwise -> return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
      Nothing | requiresForeign m -> throwError . P.errorMessage $ P.MissingFFIModule mn
              | otherwise -> return Nothing
    pjs <- P.evalSupplyT nextVar $  P.prettyPrintJS <$> (CI.moduleToCoreImp >=> (flip J.moduleToJs foreignInclude)) m
    let filePath = P.runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by psc-make version " ++ showVersion Paths.version | usePrefix]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
    writeTextFile jsFile js
    maybe (return ()) (writeTextFile foreignFile . snd) $ mn `M.lookup` foreigns
    writeTextFile externsFile exts

  requiresForeign :: CR.Module a -> Bool
  requiresForeign = not . null . CR.moduleForeign

  getTimestamp :: FilePath -> Make (Maybe UTCTime)
  getTimestamp path = makeIO (const (P.SimpleErrorWrapper $ P.CannotGetFileInfo path)) $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists

  readTextFile :: FilePath -> Make String
  readTextFile path = do
    verboseErrorsEnabled <- asks P.optionsVerboseErrors
    makeIO (const (P.SimpleErrorWrapper $ P.CannotReadFile path)) $ do
      when verboseErrorsEnabled $ putStrLn $ "Reading " ++ path
      readFile path

  writeTextFile :: FilePath -> String -> Make ()
  writeTextFile path text = makeIO (const (P.SimpleErrorWrapper $ P.CannotWriteFile path)) $ do
    mkdirp path
    putStrLn $ "Writing " ++ path
    writeFile path text
    where
    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

  progress :: String -> Make ()
  progress = liftIO . putStrLn