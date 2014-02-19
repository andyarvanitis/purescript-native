-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Main where

import Commands

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import Data.Traversable (traverse)

import System.Console.Haskeline
import System.Directory (findExecutable)
import System.Exit
import System.Environment.XDG.BaseDir
import System.Process

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U (readFile)
import qualified Text.Parsec as Parsec (Parsec, eof)

getHistoryFilename :: IO FilePath
getHistoryFilename = getUserConfigFile "purescript" "psci_history"

getPreludeFilename :: IO FilePath
getPreludeFilename = Paths.getDataFileName "prelude/prelude.purs"

findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
    where names = ["nodejs", "node"]

defaultImports :: [P.ProperName]
defaultImports = [P.ProperName "Prelude", P.ProperName "Eff"]

options :: P.Options
options = P.Options True False True (Just "Main") True "PS" []

completion :: [P.Module] -> CompletionFunc IO
completion ms = completeWord Nothing " \t\n\r" findCompletions
  where
  findCompletions :: String -> IO [Completion]
  findCompletions str = do
    files <- listFiles str
    let names = nub [ show qual
                    | P.Module moduleName ds <- ms
                    , ident <- mapMaybe getDeclName ds
                    , qual <- [ P.Qualified Nothing ident
                              , P.Qualified (Just moduleName) ident]
                    ]
    let matches = sort $ filter (isPrefixOf str) names
    return $ map simpleCompletion matches ++ files
  getDeclName :: P.Declaration -> Maybe P.Ident
  getDeclName (P.ValueDeclaration ident _ _ _) = Just ident
  getDeclName _ = Nothing

createTemporaryModule :: [P.ProperName] -> [P.DoNotationElement] -> P.Value -> P.Module
createTemporaryModule imports binders value =
  let
    moduleName = P.ModuleName [P.ProperName "Main"]
    importDecl m = P.ImportDeclaration m Nothing
    traceModule = P.ModuleName [P.ProperName "Trace"]
    trace = P.Var (P.Qualified (Just traceModule) (P.Ident "print"))
    mainDecl = P.ValueDeclaration (P.Ident "main") [] Nothing
        (P.Do (binders ++
              [ P.DoNotationBind (P.VarBinder (P.Ident "it")) value
              , P.DoNotationValue (P.App trace (P.Var (P.Qualified Nothing (P.Ident "it"))) )
              ]))
  in
    P.Module moduleName $ map (importDecl . P.ModuleName . return) imports ++ [mainDecl]

handleDeclaration :: [P.Module] -> [P.ProperName] -> [P.DoNotationElement] -> P.Value -> InputT IO ()
handleDeclaration loadedModules imports binders value = do
  let m = createTemporaryModule imports binders value
  case P.compile options (loadedModules ++ [m]) of
    Left err -> outputStrLn err
    Right (js, _, _) -> do
      process <- lift findNodeProcess
      result <- lift $ traverse (\node -> readProcessWithExitCode node [] js) process
      case result of
        Just (ExitSuccess,   out, _)   -> outputStrLn out
        Just (ExitFailure _, _,   err) -> outputStrLn err
        Nothing                        -> outputStrLn "Couldn't find node.js"

loadModule :: FilePath -> IO (Either String [P.Module])
loadModule moduleFile = do
  print moduleFile
  moduleText <- U.readFile moduleFile
  return . either (Left . show) Right $ P.runIndentParser "" P.parseModules moduleText

parseDoNotationLet :: Parsec.Parsec String P.ParseState P.DoNotationElement
parseDoNotationLet = P.DoNotationLet <$> (P.reserved "let" *> P.indented *> P.parseBinder)
                                   <*> (P.indented *> P.reservedOp "=" *> P.parseValue)

parseDoNotationBind :: Parsec.Parsec String P.ParseState P.DoNotationElement
parseDoNotationBind = P.DoNotationBind <$> P.parseBinder <*> (P.indented *> P.reservedOp "<-" *> P.parseValue)

parseExpression :: Parsec.Parsec String P.ParseState P.Value
parseExpression = P.whiteSpace *> P.parseValue <* Parsec.eof

helpMessage :: String
helpMessage = "The following commands are available:\n\n    " ++
  intercalate "\n    " (map (intercalate "    ") help)

main :: IO ()
main = do
  preludeFilename <- getPreludeFilename
  (Right prelude) <- loadModule preludeFilename
  historyFilename <- getHistoryFilename
  let settings = defaultSettings {historyFile = Just historyFilename}
  runInputT (setComplete (completion prelude) settings) $ do
    outputStrLn " ____                 ____            _       _   "
    outputStrLn "|  _ \\ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ "
    outputStrLn "| |_) | | | | '__/ _ \\___ \\ / __| '__| | '_ \\| __|"
    outputStrLn "|  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ "
    outputStrLn "|_|    \\__,_|_|  \\___|____/ \\___|_|  |_| .__/ \\__|"
    outputStrLn "                                       |_|        "
    outputStrLn ""
    outputStrLn "Expressions are terminated using Ctrl+D"
    go defaultImports prelude []
  where
  go :: [P.ProperName] -> [P.Module] -> [P.DoNotationElement] -> InputT IO ()
  go imports loadedModules binders = do
    cmd <- getCommand
    case cmd of
      Empty -> go imports loadedModules binders
      Expression ls -> do
        binders' <- case P.runIndentParser "" parseDoNotationBind (unlines ls) of
          Left _ ->
            case P.runIndentParser "" parseExpression (unlines ls) of
              Left err -> outputStrLn (show err) >> return binders
              Right decl -> do
                handleDeclaration loadedModules imports (reverse binders) decl
                return binders
          Right binder -> return $ binder:binders
        go imports loadedModules binders'
      Help -> do
        outputStrLn helpMessage
        go imports loadedModules binders
      Import moduleName ->
        go (imports ++ [P.ProperName moduleName]) loadedModules binders
      Let line -> do
        binders' <- case P.runIndentParser "" parseDoNotationLet line of
          Left err -> outputStrLn (show err) >> return binders
          Right binder -> return $ binder:binders
        go imports loadedModules binders'
      LoadModule moduleFile -> do
        ms <- lift $ loadModule moduleFile
        loadedModules' <- case ms of
          Left err -> outputStrLn err >> return loadedModules
          Right ms' -> return $ loadedModules ++ ms'
        go imports loadedModules' binders
      Reload -> do
        preludeFilename <- lift getPreludeFilename
        (Right prelude) <- lift $ loadModule preludeFilename
        go defaultImports prelude binders
      Unknown -> do
        outputStrLn "Unknown command"
        go imports loadedModules binders
