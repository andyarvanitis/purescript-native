{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
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
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime)
import System.FilePath ((</>), joinPath, splitDirectories, takeDirectory)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON

import CodeGen.Cpp
import CodeGen.Cpp.Common
import CodeGen.Cpp.Printer

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
  if null args
    then return ()
    else do
      let (opts, files) = partition (isPrefixOf "--") args
          opts' = (map . map) toLower opts
      if "--tests" `elem` opts'
        then runTests
        else do
          mapM (generateCode opts') files
          return ()

generateCode :: [String] -> FilePath -> IO ()
generateCode opts jsonFile = do
  jsonModTime <- getModificationTime jsonFile
  let filepath = takeDirectory jsonFile
      dirparts = splitDirectories $ filepath
      mname = (\c -> if c == '.' then '_' else c) <$> last dirparts
      basedir = joinPath $ init dirparts
      possInterfaceFilename = basedir </> outdir </> mname </> interfaceFileName mname
  exists <- doesFileExist possInterfaceFilename
  if exists
    then do
      modTime <- getModificationTime possInterfaceFilename
      when (modTime < jsonModTime) $
        transpile opts jsonFile
    else transpile opts jsonFile

transpile :: [String] -> FilePath -> IO ()
transpile opts jsonFile = do
  jsonText <- T.readFile jsonFile
  let module' = jsonToModule $ parseJson jsonText
  ((interface, foreigns, asts, implHeader, implFooter), _) <- runSupplyT 5 (moduleToCpp module' Nothing)
  let mn = moduleNameToCpp $ moduleName module'
      implementation = prettyPrintCpp asts
      filepath = takeDirectory jsonFile
  let baseOutpath = joinPath $ (init $ splitDirectories filepath) ++ [outdir]
      outpath = joinPath [baseOutpath, T.unpack mn]
      interfacePath = outpath </> interfaceFileName (T.unpack mn)
      implPath = outpath </> implFileName mn
  putStrLn interfacePath
  createDirectoryIfMissing True outpath
  T.writeFile interfacePath $ conv interface
  putStrLn implPath
  T.writeFile implPath $ conv (implHeader <> implementation <> implFooter)
  let runtime = baseOutpath </> "purescript.h"
  runtimeExists <- doesFileExist runtime
  when (not runtimeExists) $ do
    T.writeFile runtime . conv $ T.decodeUtf8 $(embedFile "runtime/purescript.h")
  where
  conv :: Text -> Text
  conv
    | "--ucns" `elem` opts = toUCNs . toDollars
    | otherwise = id

outdir :: FilePath
outdir = "src"

interfaceFileName :: String -> FilePath
interfaceFileName mn = mn <> ".h"

implFileName :: Text -> FilePath
implFileName mn = T.unpack $ mn <> ".cpp"

escape :: Text -> Text
escape = T.concatMap go
  where
  go :: Char -> Text
  go c = T.pack $ printf "0x%04x," (ord c)


toUCNs :: Text -> Text
toUCNs = T.pack . concatMap toUCN . T.unpack

toUCN :: Char -> String
toUCN c | isAscii c = [c]
toUCN c = printf "\\U%08x" $ ord c

toDollars :: Text -> Text
toDollars = T.map toDollar

toDollar :: Char -> Char
toDollar '＿' = '$'
toDollar c = c
