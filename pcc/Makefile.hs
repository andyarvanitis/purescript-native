-------------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2016 Andy Arvanitis and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Makefile where

import Control.Monad (when)
import Data.FileEmbed (embedFile)
import System.Environment (getExecutablePath)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

generateMakefile :: IO ()
generateMakefile = do
  makefileExists <- doesFileExist makefilename
  when (not makefileExists) $ do
    let makefile = $(embedFile "pcc/Makefile")
    exePath <- getExecutablePath
    putStrLn ""
    putStrLn $ "Generating " ++ makefilename ++ "... " ++ "pcc executable location " ++ exePath
    writeFile makefilename . T.unpack $
        T.replace
          pathPlaceholder
          (T.pack $ takeDirectory exePath)
          (T.pack $ B.unpack makefile)
    putStrLn "Done"
    putStrLn ""
    putStrLn "Run 'make' or 'make release' to build an optimized release build."
    putStrLn "Run 'make debug' to build an executable with assertions enabled and"
    putStrLn "suitable for source-level debugging."
    putStrLn ""
    putStrLn "The resulting binary executable will be located in output/bin (by default)."
    putStrLn ""

generatePackagefile :: IO ()
generatePackagefile = do
  pkgfileExists <- doesFileExist packagefilename
  when (not pkgfileExists) $ do
    let pkgfile = $(embedFile "pcc/psc-package.json")
    putStrLn ""
    putStrLn $ "Generating " ++ packagefilename ++ "..."
    writeFile packagefilename $ B.unpack pkgfile
    putStrLn "Done"
    putStrLn "Use the 'psc-package' utility to install or update packages."

generateXcodefile :: IO ()
generateXcodefile = do
  xcodefileExists <- doesFileExist xcodefilename
  when (not xcodefileExists) $ do
    let xcodefile = $(embedFile "pcc/psc-xcode.sh")
    putStrLn ""
    putStrLn $ "Generating " ++ xcodefilename ++ "..."
    writeFile xcodefilename $ B.unpack xcodefile
    putStrLn "Done"
    putStrLn "Add this file to an Xcode 'Run Script' build phase."

makefilename :: String
makefilename = "Makefile"

packagefilename :: String
packagefilename = "psc-package.json"

xcodefilename :: String
xcodefilename = "psc-xcode.sh"

pathPlaceholder :: T.Text
pathPlaceholder = "{{path}}"
