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
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer

import Data.Version (showVersion)
import qualified Data.Map as M

import Options.Applicative as Opts

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths

import Make

data PCCMakeOptions = PCCMakeOptions
  { pccmInput        :: [FilePath]
  , pccmForeignInput :: [FilePath]
  , pccmOutputDir    :: FilePath
  , pccmOpts         :: P.Options P.Make
  , pccmUsePrefix    :: Bool
  }

data InputOptions = InputOptions
  { ioNoPrelude   :: Bool
  , ioInputFiles  :: [FilePath]
  }

compile :: PCCMakeOptions -> IO ()
compile (PCCMakeOptions input [] outputDir opts usePrefix) = do
  moduleFiles <- readInput (InputOptions (P.optionsNoPrelude opts) input)
  case runWriterT (parseInputs moduleFiles []) of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
      exitFailure
    Right ((ms, _), warnings) -> do
      when (P.nonEmpty warnings) $
        hPutStrLn stderr (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings)
      let filePathMap = M.fromList $ map (\(fp, P.Module _ mn _ _) -> (mn, fp)) ms
          makeActions = buildMakeActions outputDir filePathMap usePrefix
      e <- runMake opts $ P.make makeActions ms
      case e of
        Left errs -> do
          putStrLn (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
          exitFailure
        Right (_, warnings') -> do
          when (P.nonEmpty warnings') $
            putStrLn (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings')
          exitSuccess

readInput :: InputOptions -> IO [(Either P.RebuildPolicy FilePath, String)]
readInput InputOptions{..} = forM ioInputFiles $ \inFile -> (Right inFile, ) <$> readFile inFile

parseInputs :: (Functor m, Applicative m, MonadError P.MultipleErrors m, MonadWriter P.MultipleErrors m)
            => [(Either P.RebuildPolicy FilePath, String)]
            -> [(FilePath, P.ForeignJS)]
            -> m ([(Either P.RebuildPolicy FilePath, P.Module)], M.Map P.ModuleName (FilePath, P.ForeignJS))
parseInputs modules foreigns =
  (,) <$> P.parseModulesFromFiles (either (const "") id) modules
      <*> P.parseForeignModulesFromFiles foreigns

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

inputForeignFile :: Parser FilePath
inputForeignFile = strOption $
     short 'f'
  <> long "ffi"
  <> help "The input .js file(s) providing foreign import implementations"

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

noPrelude :: Parser Bool
noPrelude = switch $
     long "no-prelude"
  <> help "Omit the automatic Prelude import"

noMagicDo :: Parser Bool
noMagicDo = switch $
     long "no-magic-do"
  <> help "Disable the optimization that overloads the do keyword to generate efficient code specifically for the Eff monad."

noOpts :: Parser Bool
noOpts = switch $
     long "no-opts"
  <> help "Skip the optimization phase."

comments :: Parser Bool
comments = switch $
     short 'c'
  <> long "comments"
  <> help "Include comments in the generated code."

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


options :: Parser (P.Options P.Make)
options = P.Options <$> noPrelude
                    <*> noTco
                    <*> noMagicDo
                    <*> pure Nothing
                    <*> noOpts
                    <*> verboseErrors
                    <*> (not <$> comments)
                    <*> pure P.MakeOptions

pccMakeOptions :: Parser PCCMakeOptions
pccMakeOptions = PCCMakeOptions <$> many inputFile
                                <*> many inputForeignFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)

main :: IO ()
main = execParser opts >>= compile
  where
  opts        = info (version <*> helper <*> pccMakeOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "pcc-make - Compiles PureScript to Javascript"
  footerInfo  = footer $ "pcc-make " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
