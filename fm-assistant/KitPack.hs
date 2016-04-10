{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Control.Monad (forM)
import Game.FMAssistant.Mod (PackFilePath(..))
import Game.FMAssistant.Repack (ArchiveFilePath(..), repackKitPack)
import Path (parent)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))

import Util (anyFailure, catchesMost)

data Command
  = Repack RepackOptions

data RepackOptions =
  RepackOptions {_outputDir :: Maybe String
                ,_repackFileNames :: [String]}

repackCmd :: Parser Command
repackCmd = Repack <$> repackOptions

repackOptions :: Parser RepackOptions
repackOptions =
  RepackOptions <$>
  optional (strOption
            (long "output-dir" <>
              short 'd' <>
              metavar "DIR" <>
              help "Repack output directory (default is same directory as kit pack)")) <*>
  some (argument str (metavar "FILE [FILE] ..."))

parser :: Parser Command
parser =
  hsubparser
    (command "repack" (info repackCmd (progDesc "Repack kit packs")))

run :: Command -> IO ExitCode
run (Repack (RepackOptions _ fns)) =
  do codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do file <- resolveFile' fp
                          let destDir = parent file
                          packFile <- repackKitPack (ArchiveFilePath file) destDir
                          putStrLn $ "Repacked " ++ fp ++ " to " ++ show (packFilePath packFile)
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
