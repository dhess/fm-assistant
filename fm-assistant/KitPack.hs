{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Control.Monad (forM, forM_, unless, void)
import Control.Monad.Catch (Handler(..), catches)
import Control.Monad.IO.Class (liftIO)
import Game.FMAssistant.Install (runReplaceMod)
import Game.FMAssistant.Mod.Kits (KitPackException, installKitPack, kpeGetFilePath, validateKitPack)
import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Version (defaultUserDirPath, runFM16)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))
import System.IO (hPrint, stderr)

import Util (anyFailure, handleIO, handleIOQuietly)

data Command
  = Install InstallOptions
  | Validate ValidateOptions

data InstallOptions =
  InstallOptions {_installFileNames :: [String]}

installCmd :: Parser Command
installCmd = Install <$> installOptions

installOptions :: Parser InstallOptions
installOptions =
  InstallOptions <$>
    some (argument str (metavar "FILE [FILE] ..."))

data ValidateOptions =
  ValidateOptions {_onlyShowInvalid :: Bool
                  ,_validateQuietly :: Bool
                  ,_validateFileNames :: [String]}

validateCmd :: Parser Command
validateCmd = Validate <$> validateOptions

validateOptions :: Parser ValidateOptions
validateOptions =
  ValidateOptions <$>
  switch (long "only-invalid" <>
          short 'i' <>
          help "Only show invalid kit packs") <*>
  switch (long "quiet" <>
          short 'q' <>
          help "Only show filenames of valid kit packs (or invalid kit packs, if --only-invalid is specified). In this mode the program always returns a 0 exit code.") <*>
  some (argument str (metavar "FILE [FILE] ..."))

parser :: Parser Command
parser =
  hsubparser
    (command "install" (info installCmd (progDesc "Install kit packs")) <>
     command "validate" (info validateCmd (progDesc "Validate kit packs")))

run :: Command -> IO ExitCode
run (Install (InstallOptions fns)) = runFM16 $
  do userDir <- defaultUserDirPath
     codes <- forM fns
                   (\fp -> liftIO $
                     catchesMost $
                       do file <- resolveFile' fp
                          runReplaceMod $ installKitPack userDir (ArchiveFilePath file)
                          putStrLn $ "Installed " ++ fp
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid False fns)) =
  do codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do file <- resolveFile' fp
                          validateKitPack (ArchiveFilePath file)
                          unless onlyInvalid $
                            putStrLn $ fp ++ ": valid kit pack"
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid True fns)) =
  do forM_ fns
           (\fp ->
             catchesMostQuietly $
               do file <- resolveFile' fp
                  void $ validateKitPack (ArchiveFilePath file)
                  unless onlyInvalid $
                    putStrLn fp)
     return ExitSuccess

handleKitPack :: KitPackException -> IO ExitCode
handleKitPack e = hPrint stderr e >> return (ExitFailure 2)

catchesMost :: IO ExitCode -> IO ExitCode
catchesMost act =
  catches act most
  where
    most = [Handler handleIO, Handler handleKitPack]

handleKitPackQuietly :: KitPackException -> IO ()
handleKitPackQuietly = putStrLn . kpeGetFilePath

catchesMostQuietly :: IO () -> IO ()
catchesMostQuietly act =
  catches act most
  where
    most = [Handler handleIOQuietly, Handler handleKitPackQuietly]
