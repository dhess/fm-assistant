{-# LANGUAGE OverloadedStrings #-}

module Trophies
       ( run
       ) where

import Game.FMAssistant.Repack.Trophies (repackTrophiesPack)
import System.Exit (ExitCode(..))

import Repack (Command)
import qualified Repack (run)

run :: Command -> IO ExitCode
run = Repack.run repackTrophiesPack
