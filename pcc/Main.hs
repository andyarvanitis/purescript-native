-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Writer.Strict

import qualified Data.Aeson as A
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Version (showVersion)

import qualified Language.PureScript as P
import           Language.PureScript.Errors.JSON

import           Options.Applicative as Opts

import qualified Paths_purescript as Paths

import qualified System.Console.ANSI as ANSI
import           System.Exit (exitSuccess, exitFailure)
import           System.FilePath.Glob (glob)
import           System.IO (hSetEncoding, hPutStrLn, stdout, stderr, utf8)
import           System.IO.UTF8 (readUTF8FileT)

import           Make
import           Makefile

data PCCMakeOptions = PCCMakeOptions
  { pccmInput        :: [FilePath]
  , pccmOutputDir    :: FilePath
  , pccmOpts         :: P.Options
  , pccmOtherOpts    :: OtherOptions
  , pccmUsePrefix    :: Bool
  , pccmJSONErrors   :: Bool
  , pccmXcode        :: Bool
  }

-- | Argumnets: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . BU8.toString . B.toStrict . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

compile :: PCCMakeOptions -> IO ()
compile PCCMakeOptions{..} = do
  input <- globWarningOnMisses (unless pccmJSONErrors . warnFileTypeNotFound) pccmInput
  when pccmXcode $ do
    generateXcodefile
  when (null input && not pccmJSONErrors) $ do
    generatePackagefile
    generateMakefile
    exitSuccess
  moduleFiles <- readInput input
  (makeErrors, makeWarnings) <- runMake pccmOpts $ do
    ms <- P.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, P.Module _ _ mn _ _) -> (mn, Right fp)) ms
    let makeActions = buildMakeActions pccmOutputDir filePathMap pccmUsePrefix pccmOtherOpts
    P.make makeActions (map snd ms)
  printWarningsAndErrors (P.optionsVerboseErrors pccmOpts) pccmJSONErrors makeWarnings makeErrors
  exitSuccess

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("pcc: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning pattern = do
    paths <- glob pattern
    when (null paths) $ warn pattern
    return paths
  concatMapM f = liftM concat . mapM f

readInput :: [FilePath] -> IO [(FilePath, Text)]
readInput inputFiles = forM inputFiles $ \inFile -> (inFile, ) <$> readUTF8FileT inFile

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

outputDirectory :: Parser FilePath
outputDirectory = strOption $
     short 'o'
  <> long "output"
  <> Opts.value "output"
  <> showDefault
  <> help "The output directory"

noTco :: Parser Bool
noTco = switch $
     long "no-tco"
  <> help "Disable tail call optimizations"

noMagicDo :: Parser Bool
noMagicDo = switch $
     long "no-magic-do"
  <> help "Disable the optimization that overloads the do keyword to generate efficient code specifically for the Eff monad"

noOpts :: Parser Bool
noOpts = switch $
     long "no-opts"
  <> help "Skip the optimization phase"

comments :: Parser Bool
comments = switch $
     short 'c'
  <> long "comments"
  <> help "Include comments in the generated code"

verboseErrors :: Parser Bool
verboseErrors = switch $
     short 'v'
  <> long "verbose-errors"
  <> help "Display verbose error messages"

noPrefix :: Parser Bool
noPrefix = switch $
     short 'p'
  <> long "no-prefix"
  <> help "Do not include comment header"

jsonErrors :: Parser Bool
jsonErrors = switch $
     long "json-errors"
  <> help "Print errors to stderr as JSON"
sourceMaps :: Parser Bool
sourceMaps = switch $
     long "source-maps"
  <> help "Generate source maps"

dumpCoreFn :: Parser Bool
dumpCoreFn = switch $
     long "dump-corefn"
  <> help "Dump the (functional) core representation of the compiled code at output/*/corefn.json"

xcode :: Parser Bool
xcode = switch $
     long "xcode"
  <> help "Generate support files for Xcode"

ucns :: Parser Bool
ucns = switch $
     long "use-ucns"
  <> help "Use UCNs for unicode identifiers (for compilers that do not fully support them, such as gcc)"

options :: Parser P.Options
options = P.Options <$> noTco
                    <*> noMagicDo
                    <*> pure Nothing
                    <*> noOpts
                    <*> verboseErrors
                    <*> (not <$> comments)
                    <*> sourceMaps
                    <*> dumpCoreFn

otherOptions :: Parser OtherOptions
otherOptions = OtherOptions <$> ucns

pccMakeOptions :: Parser PCCMakeOptions
pccMakeOptions = PCCMakeOptions <$> many inputFile
                                <*> outputDirectory
                                <*> options
                                <*> otherOptions
                                <*> (not <$> noPrefix)
                                <*> jsonErrors
                                <*> xcode

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  execParser opts >>= compile
  where
  opts        = info (version <*> helper <*> pccMakeOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "pcc - Compiles PureScript to C++11"
  footerInfo  = footer $ "pcc " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
