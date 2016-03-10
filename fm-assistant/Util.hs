{-# LANGUAGE OverloadedStrings #-}

module Util
       ( anyFailure
       , catchesMost
       , toFpList
       ) where

import Prelude hiding (FilePath)
import Control.Exception (IOException)
import Control.Monad.Catch (Handler(..), catches)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString)
import Game.FMAssistant.Mod.Kits (KitPackException)
import Game.FMAssistant.Unpack (UnpackException)
import System.Exit (ExitCode(..))
import System.IO (hPrint, stderr)

anyFailure :: ExitCode -> [ExitCode] -> ExitCode
anyFailure failCode codes =
  if all ((==) ExitSuccess) codes
     then ExitSuccess
     else failCode

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

toFpList :: [String] -> [FilePath]
toFpList = map Filesystem.decodeString
