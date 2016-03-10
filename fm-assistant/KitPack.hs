{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Prelude hiding (FilePath)
import Control.Monad (forM, forM_, unless, void)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStrLn)
import Game.FMAssistant.Mod.Kits (validateKitPack)
import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Util (fpToHumanText)
import System.Exit (ExitCode(..))

import Util (anyFailure, catchesMost, catchesMostQuietly, toFpList)

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
  do codes <- forM (toFpList fns)
                    (\fp ->
                      do T.putStrLn $ fpToHumanText fp
                         return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid False fns)) =
  do codes <- forM (toFpList fns)
                   (\fp ->
                     do exitCode <- catchesMost $
                          do void $ validateKitPack (ArchiveFilePath fp)
                             unless onlyInvalid $
                               T.putStrLn $ T.concat [fpToHumanText fp, ": valid kit pack"]
                             return ExitSuccess
                        return exitCode)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid True fns)) =
  do forM_ (toFpList fns)
           (\fp ->
             catchesMostQuietly $
               do void $ validateKitPack (ArchiveFilePath fp)
                  unless onlyInvalid $
                    T.putStrLn $ fpToHumanText fp)
     return ExitSuccess
