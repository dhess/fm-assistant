{-# LANGUAGE OverloadedStrings #-}

module RealNamesFix16
       ( run
       ) where

import Game.FMAssistant.Repack (repackRealNamesFix16)
import System.Exit (ExitCode(..))

import Repack (Command)
import qualified Repack (run)

run :: Command -> IO ExitCode
run = Repack.run repackRealNamesFix16
