{-# LANGUAGE OverloadedStrings #-}

module RepackMultiple
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Control.Monad (forM)
import Game.FMAssistant.Mod (PackFilePath(..))
import Game.FMAssistant.Repack (ArchiveFilePath(..))
import Path (parent)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))

import Repack (RepackAction)
import Util (anyFailure, catchesMost)

data Command
  = Repack RepackOptions

data RepackOptions =
  RepackOptions {_outputDir :: Maybe String
                ,_fileNames :: [String]}

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
  some (argument str (metavar "FILE"))

parser :: Parser Command
parser =
  hsubparser
    (command "repack" (info repackCmd (progDesc "Repack a mod for use with fm-assistant")))

run :: RepackAction -> Command -> IO ExitCode
run repack (Repack (RepackOptions _ fns)) =
  do codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do file <- resolveFile' fp
                          let destDir = parent file
                          packFile <- repack (ArchiveFilePath file) destDir
                          putStrLn $ "Repacked " ++ fp ++ " to " ++ show (packFilePath packFile)
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
