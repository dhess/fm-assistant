{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Prelude hiding (FilePath)
import Control.Monad (void)
import qualified Data.Text as T (concat)
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
  InstallOptions {_installFileNames :: [String]}

installCmd :: Parser Command
installCmd = Install <$> installOptions

installOptions :: Parser InstallOptions
installOptions =
  InstallOptions <$>
    some (argument str (metavar "FILE [FILE] ..."))

data ValidateOptions =
  ValidateOptions {_validateFileNames :: [String]}

validateCmd :: Parser Command
validateCmd = Validate <$> validateOptions

validateOptions :: Parser ValidateOptions
validateOptions =
  ValidateOptions <$>
    some (argument str (metavar "FILE [FILE] ..."))

parser :: Parser Command
parser =
  hsubparser
    (command "install" (info installCmd (progDesc "Install kit packs")) <>
     command "validate" (info validateCmd (progDesc "Validate kit packs")))

anyFailure :: ExitCode -> [ExitCode] -> ExitCode
anyFailure failCode codes =
  if all ((==) ExitSuccess) codes
     then ExitSuccess
     else failCode

run :: Command -> IO ExitCode
run (Install (InstallOptions fns)) =
  let fps :: [FilePath]
      fps = map Filesystem.decodeString fns
  in
    do codes <- mapM (\fp ->
                       do T.putStrLn $ fpToHumanText fp
                          return ExitSuccess)
                     fps
       return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions fns)) =
  let fps :: [FilePath]
      fps = map Filesystem.decodeString fns
  in
    do codes <- mapM (\fp ->
                       do exitCode <- catchesMost $
                            do void $ validateKitPack (ArchiveFilePath fp)
                               T.putStrLn $ T.concat [fpToHumanText fp, ": valid kit pack"]
                               return ExitSuccess
                          return exitCode)
                     fps
       return $ anyFailure (ExitFailure 1) codes
