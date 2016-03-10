{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Prelude hiding (FilePath)
import Control.Monad (forM, unless, void)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStrLn)
import Game.FMAssistant.Mod.Kits (validateKitPack)
import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Util (fpToHumanText)
import System.Exit (ExitCode(..))

import Util (anyFailure, catchesMost, toFpList)

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
                  ,_validateFileNames :: [String]}

validateCmd :: Parser Command
validateCmd = Validate <$> validateOptions

validateOptions :: Parser ValidateOptions
validateOptions =
  ValidateOptions <$>
  switch (long "only-invalid" <> help "Only show invalid kit packs") <*>
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
run (Validate (ValidateOptions onlyInvalid fns)) =
    do codes <- forM (toFpList fns)
                     (\fp ->
                       do exitCode <- catchesMost $
                            do void $ validateKitPack (ArchiveFilePath fp)
                               unless onlyInvalid $
                                 T.putStrLn $ T.concat [fpToHumanText fp, ": valid kit pack"]
                               return ExitSuccess
                          return exitCode)
       return $ anyFailure (ExitFailure 1) codes
