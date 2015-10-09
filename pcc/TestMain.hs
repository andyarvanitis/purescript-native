-------------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2015 Andy Arvanitis and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
--
--
-------------------------------------------------------------------------------
module Main where

import Data.List
import Control.Applicative
import Control.Monad

import System.Process
import System.FilePath
import System.Directory

-------------------------------------------------------------------------------
testsDir :: IO (FilePath, FilePath)
-------------------------------------------------------------------------------
testsDir = do
  baseDir <- getCurrentDirectory
  return (baseDir </> "pcc-tests", baseDir)

-------------------------------------------------------------------------------
main :: IO ()
-------------------------------------------------------------------------------
main = do

  (outputDir, baseDir) <- testsDir

  outputDirExists <- doesDirectoryExist outputDir
  when outputDirExists $ removeDirectoryRecursive outputDir
  createDirectory outputDir

  fetchPackages

  let srcDir = outputDir </> "src"
  createDirectory srcDir

  let buildDir = outputDir </> "build"
  createDirectory buildDir

  let makefile = outputDir </> "Makefile"
  writeFile makefile makefileText

  let passingDir = baseDir </> "examples" </> "passing"
  passingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents passingDir

  let tests = filter (`notElem` skipped) passingTestCases

  -- Run the tests
  --
  forM_ tests $ \inputFile -> do
    --
    -- Compile PureScript file
    --
    putStrLn $ "Compiling test " ++ inputFile ++ " ..."
    setCurrentDirectory outputDir
    copyFile (passingDir </> inputFile) (srcDir </> inputFile)
    callProcess "make" ["clean", "main"]
    --
    -- Build and run C++ files
    --
    setCurrentDirectory buildDir
    callProcess "cmake" ["../output"]
    callProcess "make" ["-j2"]
    callProcess (buildDir </> "Main") []

    removeFile (srcDir </> inputFile)

  -- TODO: support failing test cases
  --
  -- let failing = baseDir </> "examples" </> "failing"
  -- failingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents failing
  --

  setCurrentDirectory baseDir
  putStrLn "pcc-tests finished"
  putStrLn $ "Total tests available: " ++ show (length passingTestCases)
  putStrLn $ "Tests run: " ++ show (length tests)
  putStrLn $ "Tests skipped: " ++ show (length skipped)

-------------------------------------------------------------------------------
repo :: String
-------------------------------------------------------------------------------
repo = "git://github.com/pure14/"

-------------------------------------------------------------------------------
packages :: [(String, String)]
-------------------------------------------------------------------------------
packages =
  [ ("purescript-eff",       "pure14-dictionary")
  , ("purescript-prelude",   "pure14-dictionary")
  , ("purescript-assert",    "pure14-dictionary")
  , ("purescript-st",        "pure14-dictionary")
  , ("purescript-console",   "pure14-dictionary")
  , ("purescript-functions", "pure14-dictionary")
  ]

-------------------------------------------------------------------------------
fetchPackages :: IO ()
-------------------------------------------------------------------------------
fetchPackages = do
  (outputDir, baseDir) <- testsDir
  let packageDir = outputDir </> "packages"
  createDirectory packageDir
  setCurrentDirectory packageDir
  forM_ packages $ \package -> let branch = snd package in
    callProcess "git" $ ["clone"] ++ (if null branch then [] else ["--branch", branch]) ++ [repo ++ (fst package) ++ ".git"]
  setCurrentDirectory baseDir

-------------------------------------------------------------------------------
makefileText :: String
-------------------------------------------------------------------------------
makefileText = intercalate "\n" lines'
  where lines' = [ "PCC := '../.cabal-sandbox/bin/pcc'"
                 , "MODULE_DIR='packages'"
                 , "SOURCE_DIR='src'"
                 , "MODULES := $(shell find $(MODULE_DIR) -name '*.purs' | grep -v \\/test\\/ | grep -v \\/example\\/ | grep -v \\/examples\\/)"
                 , "SOURCES := $(shell find $(SOURCE_DIR) -name '*.purs')"
                 , "all: main"
                 , "main:"
                 , "\t$(PCC) $(MODULES) $(SOURCES)"
                 , "clean:"
                 , "\t@rm -rf output/*"
                 ]

-------------------------------------------------------------------------------
skipped :: [String]
-------------------------------------------------------------------------------
skipped =
  [ "ExplicitImportReExport.purs" -- OK, test has no main (would pass otherwise)
  ]
