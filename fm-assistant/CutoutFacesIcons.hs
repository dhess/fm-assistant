{-# LANGUAGE OverloadedStrings #-}

module CutoutFacesIcons
       ( run
       ) where

import Game.FMAssistant.Repack (repackCutoutIcons)
import System.Exit (ExitCode(..))

import Repack (Command)
import qualified Repack (run)

run :: Command -> IO ExitCode
run = Repack.run repackCutoutIcons
