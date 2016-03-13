{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Control.Monad (forM, forM_, unless, void)
import qualified Game.FMAssistant.FM16 as FM16 (version)
import Game.FMAssistant.Mod.Kits (installKitPack, validateKitPack)
import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Util (defaultUserDir)
import System.Exit (ExitCode(..))

import Util (anyFailure, catchesMost, catchesMostQuietly)

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
run (Install (InstallOptions fns)) =
  do userDir <- defaultUserDir FM16.version
     codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do installKitPack userDir (ArchiveFilePath fp)
                          putStrLn $ "Installed " ++ fp
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid False fns)) =
  do codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do validateKitPack (ArchiveFilePath fp)
                          unless onlyInvalid $
                            putStrLn $ fp ++ ": valid kit pack"
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid True fns)) =
  do forM_ fns
           (\fp ->
             catchesMostQuietly $
               do void $ validateKitPack (ArchiveFilePath fp)
                  unless onlyInvalid $
                    putStrLn fp)
     return ExitSuccess
