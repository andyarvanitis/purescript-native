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

import qualified Data.ByteString.Char8 as B

generateMakefile :: IO ()
generateMakefile = do
  makefileExists <- doesFileExist "Makefile"
  when (not makefileExists) $ do
    let makefile = $(embedFile "pcc/Makefile")
    exePath <- getExecutablePath
    putStrLn ""
    putStrLn $ "Generating Makefile... " ++ "pcc executable location " ++ exePath
    writeFile "Makefile" $ replace makefile pccPath exePath
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
  pkgfileExists <- doesFileExist "psc-package.json"
  when (not pkgfileExists) $ do
    let pkgfile = $(embedFile "pcc/psc-package.json")
    putStrLn ""
    putStrLn "Generating psc-package.json..."
    writeFile "psc-package.json" $ B.unpack pkgfile
    putStrLn "Done"
    putStrLn "Use the 'psc-package' utility to install or update packages."

replace :: B.ByteString -> B.ByteString -> String -> String
replace s orig repl = replace'
  where
  replace'
    | (p1, p2) <- B.breakSubstring orig s,
                  not (B.null p2) = B.unpack p1 ++ repl ++ trim (B.unpack p2)
    | otherwise = B.unpack s
  trim = drop (B.length orig)

pccPath :: B.ByteString
pccPath = "{bin/pcc}"
