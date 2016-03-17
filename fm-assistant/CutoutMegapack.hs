{-# LANGUAGE OverloadedStrings #-}

module CutoutMegapack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Control.Monad.Catch (Handler(..), catches)
import qualified Game.FMAssistant.FM16 as FM16 (defaultUserDir)
import Game.FMAssistant.Mod.Faces (FacePackException, installCutoutMegapack)
import Game.FMAssistant.Types (ArchiveFilePath(..))
import System.Exit (ExitCode(..))
import System.IO (hPrint, stderr)

import Util (handleIO)

data Command
  = Install InstallOptions

data InstallOptions =
  InstallOptions {_installFileName :: String}

installCmd :: Parser Command
installCmd = Install <$> installOptions

installOptions :: Parser InstallOptions
installOptions =
  InstallOptions <$>
    argument str (metavar "FILE")

parser :: Parser Command
parser =
  hsubparser
    (command "install" (info installCmd (progDesc "Install the Sortitoutsi Cutout Megapack (faces)")))

run :: Command -> IO ExitCode
run (Install (InstallOptions fp)) =
  do userDir <- FM16.defaultUserDir
     catchesMost $
       do installCutoutMegapack userDir (ArchiveFilePath fp)
          putStrLn $ "Installed " ++ fp
          return ExitSuccess

handleFacePack :: FacePackException -> IO ExitCode
handleFacePack e = hPrint stderr e >> return (ExitFailure 2)

catchesMost :: IO ExitCode -> IO ExitCode
catchesMost act =
  catches act most
  where
    most = [Handler handleIO, Handler handleFacePack]
