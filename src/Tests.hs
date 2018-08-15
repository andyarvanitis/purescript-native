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
  return (baseDir </> "purescript-tests", baseDir)

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

  let passingDir = baseDir </> "tests" </> "purs" </> "passing"
  passingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents passingDir

  -- Auto-generate Makefile
  setCurrentDirectory outputDir
  callProcess "cp" ["../Makefile", "."]

  fetchPackages
  callProcess "git" ["clone", "https://github.com/andyarvanitis/purescript-cpp-ffi.git", "ffi"]

  callProcess "rm" ["-rf", ".psc-package/psc-0.12.0/prelude/v4.0.0"]
  callProcess "git" ["clone", "https://github.com/andyarvanitis/purescript-prelude.git", ".psc-package/psc-0.12.0/prelude/v4.0.0"]

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
    callProcess "make" ["debug", "-j12"]
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
  putStrLn "PureScript tests finished"
  putStrLn $ "Total tests available: " ++ show (length passingTestCases)
  putStrLn $ "Tests run: " ++ show (length tests)
  putStrLn $ "Tests skipped: " ++ show (length skipped)

-------------------------------------------------------------------------------
packages :: [String]
-------------------------------------------------------------------------------
packages =
  [ "arrays"
  , "assert"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "functions"
  , "generics-rep"
  , "invariant"
  , "newtype"
  , "partial"
  , "prelude"
  , "proxy"
  , "refs"
  , "st"
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
    "EffFn.purs" -- ps-side foreign issue
  , "FunWithFunDeps.purs" -- requires FFI
  , "MultiArgFunctions.purs" -- not supported (needed?) right now
  , "NegativeIntInRange.purs" -- C++ literal restriction
  , "NumberLiterals.purs" -- unreliable float comparison, test manually
  , "PolyLabels.purs" -- ps-side foreign issue
  , "RowUnion.purs" -- ps-side foreign issue
  , "ShadowedModuleName.purs" -- ?
  , "StringEdgeCases.purs" -- ?
  , "StringEscapes.purs" -- TODO: UTF16-specific?
  ]

logpath :: FilePath
logpath = "purescript-output"

logfile :: FilePath
logfile = "purescript-tests.out"
