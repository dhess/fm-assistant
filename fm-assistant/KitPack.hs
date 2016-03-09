{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Prelude hiding (FilePath)
import Control.Monad (void)
import qualified Data.Text as T (intercalate)
import qualified Data.Text.IO as T (putStrLn)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString)
import Game.FMAssistant.Mod.Kits (validateKitPack)
import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Util (fpToHumanText)
import System.Exit (ExitCode(..))

import Util (catchesMost)

data Command
  = Install InstallOptions
  | Validate ValidateOptions

data InstallOptions =
  InstallOptions {_installFileName :: String}

installCmd :: Parser Command
installCmd = Install <$> installOptions

installOptions :: Parser InstallOptions
installOptions =
  InstallOptions <$>
    argument str (metavar "FILE")

data ValidateOptions =
  ValidateOptions {_validateFileName :: String}

validateCmd :: Parser Command
validateCmd = Validate <$> validateOptions

validateOptions :: Parser ValidateOptions
validateOptions =
  ValidateOptions <$>
    argument str (metavar "FILE")

parser :: Parser Command
parser =
  hsubparser
    (command "install" (info installCmd (progDesc "Install a kit pack")) <>
     command "validate" (info validateCmd (progDesc "Validate a kit pack")))

run :: Command -> IO ExitCode
run (Install (InstallOptions fn)) =
  let fp :: FilePath
      fp = Filesystem.decodeString fn
  in
    do T.putStrLn $ fpToHumanText fp
       return ExitSuccess
run (Validate (ValidateOptions fn)) =
  let fp :: FilePath
      fp = Filesystem.decodeString fn
  in
    do exitCode <- catchesMost $
         do void $ validateKitPack (ArchiveFilePath fp)
            T.putStrLn $ T.intercalate " " [fpToHumanText fp, "is a valid kit pack"]
            return ExitSuccess
       return exitCode
