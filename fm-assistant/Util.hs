{-# LANGUAGE OverloadedStrings #-}

module Util
       ( catchesMost
       ) where

import Control.Exception (IOException)
import Control.Monad.Catch (Handler(..), catches)
import Game.FMAssistant.Mod.Kits (KitPackException)
import Game.FMAssistant.Unpack (UnpackException)
import System.Exit (ExitCode(..))
import System.IO (hPrint, stderr)

handleIO :: IOException -> IO ExitCode
handleIO e = hPrint stderr e >> return (ExitFailure 1)

handleUnpack :: UnpackException -> IO ExitCode
handleUnpack e = hPrint stderr e >> return (ExitFailure 2)

handleKitPack :: KitPackException -> IO ExitCode
handleKitPack e = hPrint stderr e >> return (ExitFailure 3)

catchesMost :: IO ExitCode -> IO ExitCode
catchesMost action =
  catches action most
  where
    most = [Handler handleIO, Handler handleUnpack, Handler handleKitPack]
