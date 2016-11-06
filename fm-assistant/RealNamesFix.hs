{-# LANGUAGE OverloadedStrings #-}

module RealNamesFix
       ( run
       ) where

import Game.FMAssistant.Repack (repackRealNamesFix)
import System.Exit (ExitCode(..))

import Repack (Command)
import qualified Repack (run)

run :: Command -> IO ExitCode
run = Repack.run repackRealNamesFix
