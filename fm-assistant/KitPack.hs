{-# LANGUAGE OverloadedStrings #-}

module KitPack
       ( run
       ) where

import Game.FMAssistant.Repack.Kits (repackKitPack)
import System.Exit (ExitCode(..))

import RepackMultiple (Command)
import qualified RepackMultiple (run)

run :: Command -> IO ExitCode
run = RepackMultiple.run repackKitPack
