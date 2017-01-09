{-# LANGUAGE OverloadedStrings #-}

module Mod
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Control.Monad (forM, forM_, unless)
import Game.FMAssistant.Mod
       (PackFilePath(..), installMod, validateMod)
import Game.FMAssistant.Types
       (Config(..), Version(..), defaultAppDir, defaultBackupDir,
        defaultUserDir, runApp)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))

import Util (anyFailure, catchesMost, catchesMostQuietly)

data Command
  = Install InstallOptions
  | Validate ValidateOptions

data InstallOptions =
  InstallOptions {_version :: !Version
                 ,_installFileNames :: [String]}

installCmd :: Parser Command
installCmd = Install <$> installOptions

installOptions :: Parser InstallOptions
installOptions =
  InstallOptions <$>
    option auto (long "version" <>
                short 'V' <>
                metavar "VERSION" <>
                value FM17 <>
                showDefault <>
                help "FM version") <*>
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
          help "Only show invalid mod packs") <*>
  switch (long "quiet" <>
          short 'q' <>
          help "Only show filenames of valid mod packs (or invalid mod packs, if --only-invalid is specified). In this mode the program always returns a 0 exit code.") <*>
  some (argument str (metavar "FILE [FILE] ..."))

parser :: Parser Command
parser =
  hsubparser
    (command "install" (info installCmd (progDesc "Install mod packs")) <>
     command "validate" (info validateCmd (progDesc "Validate mod packs")))

run :: Command -> IO ExitCode
run (Install (InstallOptions version fns)) =
  do userDir <- defaultUserDir version
     appDir <- defaultAppDir version
     backupDir <- defaultBackupDir version
     let config = Config appDir userDir backupDir version
     codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do file <- resolveFile' fp
                          runApp config $ installMod (PackFilePath file)
                          putStrLn $ "Installed " ++ fp
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid False fns)) =
  do codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do file <- resolveFile' fp
                          validateMod (PackFilePath file)
                          unless onlyInvalid $
                            putStrLn $ fp ++ ": valid mod pack"
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid True fns)) =
  do forM_ fns
           (\fp ->
             catchesMostQuietly fp $
               do file <- resolveFile' fp
                  validateMod (PackFilePath file)
                  unless onlyInvalid $
                    putStrLn fp)
     return ExitSuccess
