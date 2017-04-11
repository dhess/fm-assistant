{-# LANGUAGE OverloadedStrings #-}

module RealNamesFix
       ( run
       ) where

import Game.FMAssistant.Repack
       (ArchiveFilePath(..), repackRealNamesFix16, repackRealNamesFix17)
import qualified Game.FMAssistant.Repack.RealNamesFix16 as FM16 (supported)
import qualified Game.FMAssistant.Repack.RealNamesFix17 as FM17 (supported)
import Path.IO (resolveFile')
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)

import Repack (Command(..), RepackOptions(..))
import qualified Repack (run)

run :: Command -> IO ExitCode
run cmd@(Repack (RepackOptions _ fp)) = do
  file <- resolveFile' fp
  run' $ ArchiveFilePath file
  where
    run' fn
      | FM16.supported fn = Repack.run repackRealNamesFix16 cmd
      | FM17.supported fn = Repack.run repackRealNamesFix17 cmd
      | otherwise = do
          hPutStrLn stderr $ fp ++ " is not a supported version of the Real Names Fix mod"
          return $ ExitFailure 1
