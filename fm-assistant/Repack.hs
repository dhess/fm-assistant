{-# LANGUAGE OverloadedStrings #-}

module Repack
       ( RepackAction
       , Command
       , run
       , parser
       ) where

import Options.Applicative

import Game.FMAssistant.Mod (PackFilePath(..))
import Game.FMAssistant.Repack (ArchiveFilePath(..))
import Path (Path, Abs, Dir, parent)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))

import Util (catchesMost)

type RepackAction = ArchiveFilePath -> Path Abs Dir -> IO PackFilePath

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
              help "Repack output directory (default is same directory as original mod)")) <*>
  argument str (metavar "FILE")

parser :: Parser Command
parser =
  hsubparser
    (command "repack" (info repackCmd (progDesc "Repack a mod for use with fm-assistant")))

run :: RepackAction -> Command -> IO ExitCode
run repack (Repack (RepackOptions _ fp)) =
  catchesMost $
    do file <- resolveFile' fp
       let destDir = parent file
       packFile <- repack (ArchiveFilePath file) destDir
       putStrLn $ "Repacked " ++ fp ++ " to " ++ show (packFilePath packFile)
       return ExitSuccess
