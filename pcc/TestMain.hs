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

  -- Prebuild the packages
  --
  setCurrentDirectory outputDir
  callProcess "make" []

  let passingDir = baseDir </> "examples" </> "passing"
  passingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents passingDir

  -- Run the tests
  --
  forM_ (filter (`notElem` exclusions) passingTestCases) $ \inputFile -> do
    --
    -- Compile PureScript file
    --
    putStrLn $ "Compiling test " ++ inputFile ++ " ..."
    setCurrentDirectory outputDir
    copyFile (passingDir </> inputFile) (srcDir </> inputFile)
    callProcess "make" []
    --
    -- Build and run C++ files
    --
    setCurrentDirectory buildDir
    callProcess "cmake" ["../output"]
    callProcess "make" []
    callProcess (buildDir </> "Main") []

    removeFile (srcDir </> inputFile)

  -- TODO: support failing test cases
  --
  -- let failing = baseDir </> "examples" </> "failing"
  -- failingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents failing
  --

  setCurrentDirectory baseDir
  putStrLn "pcc-tests finished"

-------------------------------------------------------------------------------
repo :: String
-------------------------------------------------------------------------------
repo = "git://github.com/pure14/"

-------------------------------------------------------------------------------
packages :: [String]
-------------------------------------------------------------------------------
packages =
  [ "purescript-eff"
  , "purescript-prelude"
  , "purescript-assert"
  , "purescript-st"
  , "purescript-console"
--  , "purescript-functions"
  ]

-------------------------------------------------------------------------------
fetchPackages :: IO ()
-------------------------------------------------------------------------------
fetchPackages = do
  (outputDir, baseDir) <- testsDir
  let packageDir = outputDir </> "packages"
  createDirectory packageDir
  setCurrentDirectory packageDir
  forM_ packages $ \package ->
    callProcess "git" ["clone", repo ++ package ++ ".git"]
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
exclusions :: [String]
-------------------------------------------------------------------------------
exclusions =
  [ "652.purs"
  , "CaseInDo.purs"
  , "Church.purs"
  , "Collatz.purs"
  ]
