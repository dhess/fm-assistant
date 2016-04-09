{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( Command
       , run
       , parser
       ) where

import Options.Applicative

import Control.Monad (forM, forM_, unless, void)
import Control.Monad.Catch (Handler(..), catches)
import Control.Monad.Reader (runReaderT)
import Game.FMAssistant.Install (InstallConfig(..), replaceMod)
import Game.FMAssistant.Mod (PackFilePath(..))
import Game.FMAssistant.Mod.Kits (installKitPack, validateKitPack)
import Game.FMAssistant.Repack.Kits (repackKitPack)
import Game.FMAssistant.Types (ArchiveFilePath(..), Version(..), defaultUserDir)
import Path (parent)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))

import Util (anyFailure, handleFME, handleIO, handleIOQuietly)

data Command
  = Install InstallOptions
  | Validate ValidateOptions
  | Repack RepackOptions

data InstallOptions =
  InstallOptions {_installFileNames :: [String]}

installCmd :: Parser Command
installCmd = Install <$> installOptions

installOptions :: Parser InstallOptions
installOptions =
  InstallOptions <$>
    some (argument str (metavar "FILE [FILE] ..."))

data ValidateOptions =
  ValidateOptions {_onlyShowInvalid :: Bool
                  ,_validateQuietly :: Bool
                  ,_validateFileNames :: [String]}

validateCmd :: Parser Command
validateCmd = Validate <$> validateOptions

validateOptions :: Parser ValidateOptions
validateOptions =
  ValidateOptions <$>
  switch (long "only-invalid" <>
          short 'i' <>
          help "Only show invalid kit packs") <*>
  switch (long "quiet" <>
          short 'q' <>
          help "Only show filenames of valid kit packs (or invalid kit packs, if --only-invalid is specified). In this mode the program always returns a 0 exit code.") <*>
  some (argument str (metavar "FILE [FILE] ..."))

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
    (command "install" (info installCmd (progDesc "Install kit packs")) <>
     command "validate" (info validateCmd (progDesc "Validate kit packs")) <>
     command "repack" (info repackCmd (progDesc "Repack kit packs")))

run :: Command -> IO ExitCode
run (Install (InstallOptions fns)) =
  do userDir <- defaultUserDir FM16
     codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do file <- resolveFile' fp
                          runReaderT
                            (installKitPack (ArchiveFilePath file))
                            (InstallConfig userDir replaceMod)
                          putStrLn $ "Installed " ++ fp
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
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
run (Validate (ValidateOptions onlyInvalid False fns)) =
  do codes <- forM fns
                   (\fp ->
                     catchesMost $
                       do file <- resolveFile' fp
                          validateKitPack (ArchiveFilePath file)
                          unless onlyInvalid $
                            putStrLn $ fp ++ ": valid kit pack"
                          return ExitSuccess)
     return $ anyFailure (ExitFailure 1) codes
run (Validate (ValidateOptions onlyInvalid True fns)) =
  do forM_ fns
           (\fp ->
             catchesMostQuietly fp $
               do file <- resolveFile' fp
                  void $ validateKitPack (ArchiveFilePath file)
                  unless onlyInvalid $
                    putStrLn fp)
     return ExitSuccess

catchesMost :: IO ExitCode -> IO ExitCode
catchesMost act =
  catches act most
  where
    most = [Handler handleIO, Handler handleFME]

catchesMostQuietly :: FilePath -> IO () -> IO ()
catchesMostQuietly fp act =
  catches act most
  where
    most = [Handler (handleIOQuietly fp), Handler (handleIOQuietly fp)]
