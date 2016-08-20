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

  let passingDir = baseDir </> "examples" </> "passing"
  passingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents passingDir

  -- Auto-generate Makefile
  setCurrentDirectory outputDir
  callProcess "pcc" []

  let tests = filter (`notElem` skipped) passingTestCases

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
    callProcess "make" ["debug"]
    --
    -- Run C++ files
    --
    callProcess ("output" </> "bin" </> "main") []

    removeFile (srcDir </> inputFile)
    when testCaseDirExists $ callProcess "rm" ["-rf", srcDir </> (takeWhile (/='.') inputFile)]

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
repo = "git://github.com/pure11/"

-------------------------------------------------------------------------------
packages :: [(String, String)]
-------------------------------------------------------------------------------
packages =
  [ ("purescript-eff",       "")
  , ("purescript-prelude",   "")
  , ("purescript-assert",    "")
  , ("purescript-st",        "")
  , ("purescript-console",   "")
  , ("purescript-functions", "")
  , ("purescript-partial",   "")
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
skipped :: [String]
-------------------------------------------------------------------------------
skipped =
  [ "NumberLiterals.purs" -- unreliable float comparison, test manually
  ]
