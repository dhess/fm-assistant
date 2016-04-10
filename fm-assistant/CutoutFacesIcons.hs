{-# LANGUAGE OverloadedStrings #-}

module CutoutFacesIcons
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Game.FMAssistant.Mod (PackFilePath(..))
import Game.FMAssistant.Repack (ArchiveFilePath(..), repackCutoutIcons)
import Path (parent)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))

import Util (catchesMost)

data Command
  = Repack RepackOptions

data RepackOptions =
  RepackOptions {_outputDir :: Maybe String
                ,_fileName :: String}

repackCmd :: Parser Command
repackCmd = Repack <$> repackOptions

repackOptions :: Parser RepackOptions
repackOptions =
  RepackOptions <$>
  optional (strOption
            (long "output-dir" <>
              short 'd' <>
              metavar "DIR" <>
              help "Repack output directory (default is same directory as face pack)")) <*>
  argument str (metavar "FILE")

parser :: Parser Command
parser =
  hsubparser
    (command "repack" (info repackCmd (progDesc "Repack the Sortitoutsi Cutout Icons")))

run :: Command -> IO ExitCode
run (Repack (RepackOptions _ fp)) =
  catchesMost $
    do file <- resolveFile' fp
       let destDir = parent file
       packFile <- repackCutoutIcons (ArchiveFilePath file) destDir
       putStrLn $ "Repacked " ++ fp ++ " to " ++ show (packFilePath packFile)
       return ExitSuccess
