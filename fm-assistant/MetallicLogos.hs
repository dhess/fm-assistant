{-# LANGUAGE OverloadedStrings #-}

module MetallicLogos
       ( run
       ) where

import Game.FMAssistant.Repack (repackMetallicLogos)
import System.Exit (ExitCode(..))

import Repack (Command)
import qualified Repack (run)

run :: Command -> IO ExitCode
run = Repack.run repackMetallicLogos
