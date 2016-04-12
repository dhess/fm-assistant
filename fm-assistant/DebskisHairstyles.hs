{-# LANGUAGE OverloadedStrings #-}

module DebskisHairstyles
       ( run
       ) where

import Game.FMAssistant.Repack (repackDebskisHairstyles)
import System.Exit (ExitCode(..))

import Repack (Command)
import qualified Repack (run)

run :: Command -> IO ExitCode
run = Repack.run repackDebskisHairstyles
