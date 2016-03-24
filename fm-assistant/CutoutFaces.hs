{-# LANGUAGE OverloadedStrings #-}

module CutoutFaces
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Control.Monad.Catch (Handler(..), catches)
import Control.Monad.IO.Class (liftIO)
import Game.FMAssistant.Install (runReplaceMod)
import Game.FMAssistant.Mod.Faces (FacePackException, installCutoutMegapack, installCutoutIcons)
import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Version (defaultUserDirPath, runFM16)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))
import System.IO (hPrint, stderr)

import Util (handleIO)

data Command
  = InstallMegapack InstallOptions
  | InstallIcons InstallOptions

data InstallOptions =
  InstallOptions {_fileName :: String}

installMegapackCmd :: Parser Command
installMegapackCmd = InstallMegapack <$> installOptions

installIconsCmd :: Parser Command
installIconsCmd = InstallIcons <$> installOptions

installOptions :: Parser InstallOptions
installOptions =
  InstallOptions <$>
    argument str (metavar "FILE")

parser :: Parser Command
parser =
  hsubparser
    (command "install-megapack" (info installMegapackCmd (progDesc "Install the Sortitoutsi Cutout Megapack")) <>
     command "install-icons" (info installIconsCmd (progDesc "Install the Sortitoutsi Cutout Icon pack")))

run :: Command -> IO ExitCode
run (InstallMegapack (InstallOptions fp)) = runFM16 $
  do userDir <- defaultUserDirPath
     liftIO $
       catchesMost $
         do file <- resolveFile' fp
            runReplaceMod $ installCutoutMegapack userDir (ArchiveFilePath file)
            putStrLn $ "Installed " ++ fp
            return ExitSuccess
run (InstallIcons (InstallOptions fp)) = runFM16 $
  do userDir <- defaultUserDirPath
     liftIO $
       catchesMost $
         do file <- resolveFile' fp
            runReplaceMod $ installCutoutIcons userDir (ArchiveFilePath file)
            putStrLn $ "Installed " ++ fp
            return ExitSuccess

handleFacePack :: FacePackException -> IO ExitCode
handleFacePack e = hPrint stderr e >> return (ExitFailure 2)

catchesMost :: IO ExitCode -> IO ExitCode
catchesMost act =
  catches act most
  where
    most = [Handler handleIO, Handler handleFacePack]
