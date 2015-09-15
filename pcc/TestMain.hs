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
  putStrLn $ "Total tests available: " ++ show (length passingTestCases)
  putStrLn $ "Tests run: " ++ show (length tests)
  putStrLn $ "Tests skipped: " ++ show (length skipped)

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
  , "purescript-functions"
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
skipped :: [String]
-------------------------------------------------------------------------------
skipped =
  [ "862.purs"
  , "Auto.purs"           -- rank-N
  , "CheckTypeClass.purs" -- rank-N
  , "Church.purs"         -- rank-N
  , "Collatz.purs"        -- rank-N (runPure)
  , "Do.purs"             -- rank-N, also mutually recursive lets
  , "Dollar.purs"         -- rank-N (inferred)
  , "Eff.purs"            -- rank-N (runPure)
  , "EmptyDataDecls.purs"
  , "ExplicitImportReExport.purs" -- ok, test has no main (would pass otherwise)
  , "Fib.purs"                 -- rank-N (runST)
  , "FinalTagless.purs"        -- rank-N
  , "Let.purs"                 -- rank-N
  , "LiberalTypeSynonyms.purs" -- rank-N
  , "Monad.purs"               -- rank-N
  , "MonadState.purs"
  , "MultiArgFunctions.purs" -- uses package purescript-functions
  , "MutRec2.purs" -- incompatible test, uses foreign data (would pass otherwise)
  , "MutRec3.purs" -- incompatible test, uses foreign data (would pass otherwise)
  , "Nested.purs"
  , "OperatorInlining.purs" -- excessive memory consumption bug
  , "OverlappingInstances.purs"  -- ok, should fail with C++
  , "OverlappingInstances2.purs" -- ok, should fail with C++
  , "Rank2Data.purs"        -- rank-N
  , "Rank2Object.purs"      -- rank-N
  , "Rank2TypeSynonym.purs" -- rank-N
  , "Rank2Types.purs"       -- rank-N
  , "RebindableSyntax.purs"
  , "RowPolyInstanceContext.purs"
  , "ScopedTypeVariables.purs" -- rank-N
  , "SequenceDesugared.purs"   -- rank-N
  , "TypeClasses.purs"
  , "TypedWhere.purs" -- rank-N
  , "Where.purs"      -- rank-N
  ]
