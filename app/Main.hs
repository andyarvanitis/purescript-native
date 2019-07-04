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
import Data.List (delete, isPrefixOf, partition)
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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString as B

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON

import CodeGen.IL
import CodeGen.IL.Common
import CodeGen.IL.Printer

import Tests

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
      when ("--makefile" `elem` opts') $
        B.writeFile "Makefile" $(embedFile "support/Makefile")
      when ("--help" `elem` opts') $
        putStrLn help
      if null files then do
        currentDir <- getCurrentDirectory
        processFiles opts' [currentDir]
      else do
        processFiles opts' files
      return ()
  where
  processFiles :: [String] -> [FilePath] -> IO ()
  processFiles opts [file] = do
    isDir <- doesDirectoryExist file
    if isDir then do
      files <- find always (fileName ==? corefn) file
      if null files then do
        noneFound
      else do
        generateFiles opts files
    else do
      generateFiles opts [file]
  processFiles opts files = do
    let baseOutpath = basePath files
    writeRuntimeFiles baseOutpath
    Par.mapM (generateCode opts baseOutpath) files
    return ()
  basePath :: [FilePath] -> FilePath
  basePath files =
    let filepath = takeDirectory (head files) in
    joinPath $ (init $ splitDirectories filepath) ++ [outdir]
  generateFiles :: [String] -> [FilePath] -> IO ()
  generateFiles opts files = do
    let baseOutpath = basePath files
    writeRuntimeFiles baseOutpath
    Par.mapM (generateCode opts baseOutpath) files
    return ()

generateCode :: [String] -> FilePath -> FilePath -> IO ()
generateCode opts baseOutpath jsonFile = do
  jsonModTime <- getModificationTime jsonFile
  let filepath = takeDirectory jsonFile
      dirparts = splitDirectories $ filepath
      mname = (\c -> if c == '.' then '_' else c) <$> last dirparts
      basedir = joinPath $ init dirparts
      mname' = moduleNameToIL' $ T.pack mname
      possibleFileName = basedir </> outdir </> (T.unpack mname') </> implFileName mname'
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
  let module' = jsonToModule $ parseJson jsonText
  ((_, foreigns, asts, implHeader, implFooter), _) <- runSupplyT 5 (moduleToIL module' Nothing)
  let mn = moduleNameToIL $ moduleName module'
      implementation = prettyPrintIL asts
      outpath = joinPath [baseOutpath, T.unpack mn]
      implPath = outpath </> implFileName mn
  createDirectoryIfMissing True outpath
  putStrLn implPath
  B.writeFile implPath $ T.encodeUtf8 (implHeader <> implementation <> implFooter)

writeRuntimeFiles :: FilePath -> IO ()
writeRuntimeFiles baseOutpath = do
  createDirectoryIfMissing True $ baseOutpath </> "purescript"
  let runtimeSource = baseOutpath </> "purescript" </> "purescript.go"
  runtimeExists <- doesFileExist runtimeSource
  when (not runtimeExists) $ do
    B.writeFile runtimeSource $(embedFile "runtime/purescript.go")

outdir :: FilePath
outdir = "src"

implFileName :: Text -> FilePath
implFileName mn = T.unpack $ mn <> ".go"

help :: String
help = "Usage: psgo OPTIONS COREFN-FILES\n\
       \  PureScript-to-go compiler\n\n\
       \Available options:\n\
       \  --help                  Show this help text\n\n\
       \  --makefile              Generate a GNU Makefile which can be used for compiling\n\
       \                          a PureScript program and libraries to a native binary via\n\
       \                          purs corefn output and C++\n\n\
       \  --tests                 Run test cases (under construction)\n\n\
       \See also:\n\
       \  purs compile --help\n"

corefn :: String
corefn = "corefn.json"

noneFound :: IO ()
noneFound = do
    putStrLn $ "No compiled purescript files (" <> corefn <> ") found"
