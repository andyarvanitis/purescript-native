-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds, DoAndIfThenElse, GeneralizedNewtypeDeriving, TupleSections #-}

module Main (main) where

import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C

import Data.List (isSuffixOf)
import Data.Traversable (for, traverse)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT)
import Control.Applicative
import Control.Arrow (first)
import System.Exit
import System.Process
import System.FilePath ((</>), pathSeparator, takeDirectory)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable,
                         getCurrentDirectory, getDirectoryContents, getModificationTime,
                         getTemporaryDirectory, setCurrentDirectory)
import System.IO.Error (tryIOError)

newtype Make a = Make { unMake :: ReaderT (P.Options P.Make) (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader (P.Options P.Make))

runMake :: P.Options P.Make -> Make a -> IO (Either String a)
runMake opts = runExceptT . flip runReaderT opts . unMake

makeIO :: IO a -> Make a
makeIO = Make . lift . ExceptT . fmap (either (Left . show) Right) . tryIOError

instance P.MonadMake Make where
  getTimestamp path = makeIO $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists
  readTextFile path = makeIO $ do
    putStrLn $ "Reading " ++ path
    readFile path
  writeTextFile path text = makeIO $ do
    createDirectoryIfMissing True $ takeDirectory path
    putStrLn $ "Writing " ++ path
    writeFile path text
  progress = makeIO . putStrLn

readInput :: [FilePath] -> IO [(FilePath, String)]
readInput inputFiles = forM inputFiles $ \inputFile -> do
  text <- readFile inputFile
  return (inputFile, text)

compile :: P.Options P.Make -> [FilePath] -> IO (Either String (FilePath, P.Environment))
compile opts inputFiles = do
  modules <- P.parseModulesFromFiles id . ((C.prelude, P.prelude) :) <$> readInput inputFiles
  let outputDir = "test-output"
  case modules of
    Left parseError ->
      return (Left $ show parseError)
    Right ms -> fmap (fmap (outputDir, )) $ runMake opts $ P.make outputDir (map (first Right) ms) []

assert :: P.Options P.Make -> FilePath -> (Either String (FilePath, P.Environment) -> IO (Maybe String)) -> IO ()
assert opts inputFile f = do
  e <- compile opts [inputFile]
  maybeErr <- f e
  case maybeErr of
    Just err -> putStrLn err >> exitFailure
    Nothing -> return ()

assertCompiles :: FilePath -> IO ()
assertCompiles inputFile = do
  putStrLn $ "Assert " ++ inputFile ++ " compiles successfully"
  let options = P.defaultMakeOptions
  assert options inputFile $ either (return . Just) $ \(outputDir, _) -> do
    mexes <- runMaybeT $ (,) <$> findOneExecutableIn ["cmake"] <*> findOneExecutableIn ["make"]

    let buildDir = outputDir </> "build"
    origCwd <- getCurrentDirectory
    print (origCwd, outputDir, buildDir)
    createDirectoryIfMissing True buildDir
    setCurrentDirectory buildDir

    let handleExitStatus s cont = case s of
          (ExitFailure _, _, err) -> return $ Just err
          (ExitSuccess, out, _) -> putStrLn out >> cont

    result <- for mexes $ \(cmake, make) -> do
      cmakeResult <- readProcessWithExitCode cmake [".."] ""
      handleExitStatus cmakeResult $ do
        makeResult <- readProcessWithExitCode make [] ""
        handleExitStatus makeResult (return Nothing)

    setCurrentDirectory origCwd

    case result of
      Just merr -> return merr
      Nothing -> return $ Just "Couldn't find node.js executable"

assertDoesNotCompile :: FilePath -> IO ()
assertDoesNotCompile inputFile = do
  putStrLn $ "Assert " ++ inputFile ++ " does not compile"
  assert P.defaultMakeOptions inputFile $ \e ->
    case e of
      Left err -> putStrLn err >> return Nothing
      Right _ -> return $ Just "Should not have compiled"

findOneExecutableIn :: [String] -> MaybeT IO String
findOneExecutableIn = msum . map (MaybeT . findExecutable)

main :: IO ()
main = do
  cd <- getCurrentDirectory
  let examples = cd ++ pathSeparator : "examples"
  let passing = examples ++ pathSeparator : "passing"
  passingTestCases <- getDirectoryContents passing
  forM_ passingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
    assertCompiles (passing ++ pathSeparator : inputFile)
  let failing = examples ++ pathSeparator : "failing"
  failingTestCases <- getDirectoryContents failing
  forM_ failingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
    assertDoesNotCompile (failing ++ pathSeparator : inputFile)
  exitSuccess

