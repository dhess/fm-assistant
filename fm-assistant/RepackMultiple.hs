{-# LANGUAGE OverloadedStrings #-}

module RepackMultiple
       ( Command
       , parser
       , run
       ) where

import Options.Applicative

import Control.Monad (forM)
import System.Exit (ExitCode(..))

import Repack (RepackAction, repackFile)
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
  do codes <- forM fns (catchesMost . repackFile repack)
     return $ anyFailure (ExitFailure 1) codes
