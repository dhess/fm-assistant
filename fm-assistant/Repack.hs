{-# LANGUAGE OverloadedStrings #-}

module Repack
       ( RepackAction
       , Command
       , parser
       , repackFile
       , run
       ) where

import Options.Applicative

import Game.FMAssistant.Mod (PackFilePath(..))
import Game.FMAssistant.Repack (ArchiveFilePath(..))
import Path (Path, Abs, Dir)
import Path.IO (resolveFile', resolveDir')
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
              help "Repack output directory (default is current directory)")) <*>
  argument str (metavar "FILE")

parser :: Parser Command
parser =
  hsubparser
    (command "repack" (info repackCmd (progDesc "Repack a mod for use with fm-assistant")))

run :: RepackAction -> Command -> IO ExitCode
run repack (Repack (RepackOptions outputDir fp)) =
  do destDir <- case outputDir of
                  Nothing -> resolveDir' "."
                  Just dir -> resolveDir' dir
     catchesMost $ repackFile repack destDir fp

repackFile :: RepackAction -> Path Abs Dir -> FilePath -> IO ExitCode
repackFile repack outputDir fp =
  do file <- resolveFile' fp
     packFile <- repack (ArchiveFilePath file) outputDir
     putStrLn $ "Repacked " ++ fp ++ " to " ++ show (packFilePath packFile)
     return ExitSuccess
