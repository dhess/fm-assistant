{-# LANGUAGE OverloadedStrings #-}

module CutoutFacesMegapack
       ( run
       ) where

import Game.FMAssistant.Repack (repackCutoutMegapack)
import System.Exit (ExitCode(..))

import Repack (Command)
import qualified Repack (run)

run :: Command -> IO ExitCode
run = Repack.run repackCutoutMegapack
