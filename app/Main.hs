{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
import qualified Control.Monad.Parallel as Par
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.FileEmbed (embedFile)
import Data.List (delete, intercalate, isPrefixOf, nub, partition, (\\))
import Data.Maybe
import Data.Monoid ((<>))
import Data.Version
import Control.Monad.Supply
import Control.Monad.Supply.Class
import Text.Printf

import System.Environment
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getModificationTime)
import System.FilePath ((</>), joinPath, splitDirectories, takeDirectory)
import System.FilePath.Find
import System.Process

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString as B

import Development.GitRev

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON

import CodeGen.IL
import CodeGen.IL.Common
import CodeGen.IL.Printer

import Tests

data Command = Build | Run

parseJson :: Text -> Value
parseJson text
  | Just fileJson <- decode . L.encodeUtf8 $ L.fromStrict text = fileJson
  | otherwise = error "Bad json"

jsonToModule :: Value -> Module Ann
jsonToModule value =
  case parse moduleFromJSON value of
    Success (_, r) -> r
    _ -> error "failed"                  

main :: IO ()
main = do
  args <- getArgs
  let (opts, files) = partition (isPrefixOf "--") args
      opts' = (map . map) toLower opts
  if "--tests" `elem` opts'
    then runTests
    else do
      if "--help" `elem` opts' || "--version" `elem` opts'
        then do
          when ("--help" `elem` opts') $ do
            putStrLn help
          when ("--version" `elem` opts') $ do
            let branch = $(gitBranch)
                details | branch == "golang" = "master, commit " ++ $(gitHash)
                        | otherwise = branch
            putStrLn $ details ++ if $(gitDirty) then " (DIRTY)" else ""
        else do
          if null files
            then do
              currentDir <- getCurrentDirectory
              processFiles opts' [currentDir]
            else
              processFiles opts' files
          if "--run" `elem` opts
            then runBuildTool Run
            else when ("--no-build" `notElem` opts) $
                 runBuildTool Build
  return ()
  where
  processFiles :: [String] -> [FilePath] -> IO ()
  processFiles opts [file] = do
    isDir <- doesDirectoryExist file
    if isDir then do
      files <- find always (fileName ==? corefn) file
      if null files
        then errorNoCorefnFiles
        else generateFiles opts files
    else generateFiles opts [file]
  processFiles opts files = do
    generateFiles opts files
  basePath :: [FilePath] -> FilePath
  basePath files =
    let filepath = takeDirectory (head files) in
    joinPath $ (init $ splitDirectories filepath)
  generateFiles :: [String] -> [FilePath] -> IO ()
  generateFiles opts files = do
    let baseOutpath = basePath files
    writeSupportFiles baseOutpath
    Par.mapM (generateCode opts baseOutpath) files
    return ()
  runBuildTool :: Command -> IO ()
  runBuildTool cmd = do
    let command = case cmd of
                    Build -> "build"
                    Run -> "run"
    project <- envVar goproject
    (_, _, _, p) <- createProcess (proc "go" [command, T.unpack $ project <> modPrefix <> "/Main"])
    waitForProcess p
    return ()

generateCode :: [String] -> FilePath -> FilePath -> IO ()
generateCode opts baseOutpath jsonFile = do
  jsonModTime <- getModificationTime jsonFile
  let filepath = takeDirectory jsonFile
      dirparts = splitDirectories $ filepath
      mname = last dirparts
      basedir = joinPath $ init dirparts
      mname' = T.pack mname
      possibleFileName = basedir </> (T.unpack mname') </> implFileName mname'
  exists <- doesFileExist possibleFileName
  if exists
    then do
      modTime <- getModificationTime possibleFileName
      when (modTime < jsonModTime) $
        transpile opts baseOutpath jsonFile
    else transpile opts baseOutpath jsonFile

transpile :: [String] -> FilePath -> FilePath -> IO ()
transpile opts baseOutpath jsonFile = do
  jsonText <- T.decodeUtf8 <$> B.readFile jsonFile
  project <- envVar goproject
  let module' = jsonToModule $ parseJson jsonText
  ((_, foreigns, asts, implHeader, implFooter), _) <- runSupplyT 5 (moduleToIL module' project)
  let mn = moduleNameToIL' $ moduleName module'
      implementation = prettyPrintIL asts
      outpath = joinPath [baseOutpath, T.unpack mn]
      implPath = outpath </> implFileName mn
  createDirectoryIfMissing True outpath
  putStrLn implPath
  B.writeFile implPath $ T.encodeUtf8 (implHeader <> implementation <> implFooter)

writeSupportFiles :: FilePath -> IO ()
writeSupportFiles baseOutpath = do
  currentDir <- getCurrentDirectory
  let outputdir = T.pack $ baseOutpath \\ currentDir
      ffiOutpath = currentDir </> "purescript-native"
  createDirectoryIfMissing True baseOutpath
  createDirectoryIfMissing True ffiOutpath
  writeModuleFile outputdir currentDir  $(embedFile "support/go.mod.working")
  writeModuleFile outputdir baseOutpath $(embedFile "support/go.mod.output")
  writeModuleFile outputdir ffiOutpath  $(embedFile "support/go.mod.ffi-loader")
  writeLoaderFile ffiOutpath $(embedFile "support/ffi-loader.go")
  where
  writeModuleFile :: Text -> FilePath -> B.ByteString -> IO ()
  writeModuleFile outputdir path modText = do
    let goModSource = path </> "go.mod"
    goModSourceExists <- doesFileExist goModSource
    when (not goModSourceExists) $ do
      project <- envVar goproject
      let modText' = T.replace "/$OUTPUT/" outputdir $ T.decodeUtf8 modText
          modText'' = T.replace "$PROJECT/" project modText'
      B.writeFile goModSource $ T.encodeUtf8 modText''
  writeLoaderFile :: FilePath -> B.ByteString -> IO ()
  writeLoaderFile ffiOutpath loaderText = do
    let loaderSource = ffiOutpath </> "ffi_loader.go"
    loaderSourceExists <- doesFileExist loaderSource
    when (not loaderSourceExists) $ do
      B.writeFile loaderSource loaderText

implFileName :: Text -> FilePath
implFileName mn = ((\c -> if c == '.' then '_' else c) <$> T.unpack mn) <> ".go"

envVar :: String -> IO Text
envVar var = do
  T.pack . maybe "" (<> "/") <$> lookupEnv var

help :: String
help = "Usage: psgo OPTIONS COREFN-FILES\n\
       \  PureScript to native (via go) compiler\n\n\
       \Available options:\n\
       \  --help                  Show this help text\n\n\
       \  --version               Show the version number\n\n\
       \  --run                   Run the generated go code directly, without building an\n\
       \                          executable\n\
       \  --no-build              Generate go source files, but do not build an executable\n\
       \  --tests                 Run test cases (under construction)\n\n\
       \See also:\n\
       \  purs compile --help\n"

corefn :: String
corefn = "corefn.json"

goSrc :: String
goSrc = ".go"

goproject :: String
goproject = "GOPROJECT"

errorNoCorefnFiles :: IO ()
errorNoCorefnFiles = do
    ioError . userError $ "no compiled purescript '" <> corefn <> "' files found –\n" <>
        "                  make sure to use the '--codegen corefn' option with your purs\n" <>
        "                  project build tool"
