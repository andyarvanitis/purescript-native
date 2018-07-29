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
module Tests (runTests) where

import Prelude.Compat
import Data.List
import Control.Applicative
import Control.Monad

import System.Process
import System.FilePath
import System.IO
import System.Directory

-------------------------------------------------------------------------------
testsDir :: IO (FilePath, FilePath)
-------------------------------------------------------------------------------
testsDir = do
  baseDir <- getCurrentDirectory
  return (baseDir </> "objc-tests", baseDir)

-------------------------------------------------------------------------------
runTests :: IO ()
-------------------------------------------------------------------------------
runTests = do

  (outputDir, baseDir) <- testsDir

  outputDirExists <- doesDirectoryExist outputDir
  when outputDirExists $ removeDirectoryRecursive outputDir
  createDirectory outputDir

  let srcDir = outputDir </> "src"
  createDirectory srcDir

  let passingDir = baseDir </> "examples" </> "passing"
  passingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents passingDir

  -- Auto-generate Makefile
  setCurrentDirectory outputDir
  callProcess "cp" ["../Makefile", "."]

  fetchPackages

  let tests = filter (`notElem` skipped) passingTestCases

  tmp <- getTemporaryDirectory
  createDirectoryIfMissing False (tmp </> logpath)
  outputFile <- openFile (tmp </> logpath </> logfile) WriteMode
  hClose outputFile

  -- Run the tests
  --
  forM_ tests $ \inputFile -> do
    --
    -- Compile/build
    --
    putStrLn $ "Compiling test " ++ inputFile ++ " ..."
    setCurrentDirectory outputDir
    copyFile (passingDir </> inputFile) (srcDir </> inputFile)

    let testCaseDir = passingDir </> (takeWhile (/='.') inputFile)
    testCaseDirExists <- doesDirectoryExist testCaseDir
    when testCaseDirExists $ callProcess "cp" ["-R", testCaseDir, srcDir]

    callProcess "make" ["clean"]
    callProcess "make" ["release", "-j12"]
    --
    -- Run C++ files
    --
    outputFile <- openFile (tmp </> logpath </> logfile) AppendMode
    hPutStrLn outputFile ("\n" ++ inputFile ++ ":")
    proc <- runProcess ("output" </> "bin" </> "main") [] Nothing Nothing Nothing (Just outputFile) Nothing

    removeFile (srcDir </> inputFile)
    when testCaseDirExists $ callProcess "rm" ["-rf", srcDir </> (takeWhile (/='.') inputFile)]

  -- TODO: support failing test cases
  --
  -- let failing = baseDir </> "examples" </> "failing"
  -- failingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents failing
  --

  setCurrentDirectory baseDir
  putStrLn "objc-tests finished"
  putStrLn $ "Total tests available: " ++ show (length passingTestCases)
  putStrLn $ "Tests run: " ++ show (length tests)
  putStrLn $ "Tests skipped: " ++ show (length skipped)

-------------------------------------------------------------------------------
packages :: [String]
-------------------------------------------------------------------------------
packages =
  [ "eff"
  , "arrays"
  , "assert"
  , "console"
  , "control"
  , "foldable-traversable"
  , "functions"
  , "generics-rep"
  , "invariant"
  , "monoid"
  , "newtype"
  , "partial"
  , "prelude"
  , "proxy"
  , "refs"
  , "st"
  , "symbols"
  , "type-equality"
  , "typelevel-prelude"
  ]

-------------------------------------------------------------------------------
fetchPackages :: IO ()
-------------------------------------------------------------------------------
fetchPackages = do
  callProcess "psc-package" ["init"]
  mapM (callProcess "psc-package" . (\p -> ["install", p])) packages
  return ()

-------------------------------------------------------------------------------
skipped :: [String]
-------------------------------------------------------------------------------
skipped =
  [ 
    "NumberLiterals.purs" -- unreliable float comparison, test manually
  , "2172.purs" -- ps-side foreign issue
  , "AppendInReverse.purs" -- test not updated to post-0.12?
  , "EffFn.purs" -- ps-side foreign issue
  , "MultiArgFunctions.purs" -- not supported (needed?) right now
  , "NegativeIntInRange.purs" -- supposed to fail?
  , "PolyLabels.purs" -- ps-side foreign issue
  , "RowUnion.purs" -- ps-side foreign issue
  , "ShadowedModuleName.purs" -- ?
  , "FunWithFunDeps.purs" -- requires FFI
  , "StringEdgeCases.purs" -- ?
  , "StringEscapes.purs" -- TODO: UTF16-specific?
  ]

logpath :: FilePath
logpath = "purescript-output"

logfile :: FilePath
logfile = "objc-tests.out"
