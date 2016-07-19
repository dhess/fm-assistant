{-# LANGUAGE OverloadedStrings #-}

module Skin
       ( run
       ) where

import Game.FMAssistant.Repack.Skins (repackSkin)
import System.Exit (ExitCode(..))

import RepackMultiple (Command)
import qualified RepackMultiple (run)

run :: Command -> IO ExitCode
run = RepackMultiple.run repackSkin
