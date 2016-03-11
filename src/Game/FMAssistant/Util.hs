{-|
Module      : Game.FMAssistant.Util
Description : Utility functions & combinators
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Helpful functions and combinators.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Util
       ( defaultUserDir
       , mktempdir
       , executeQuietly
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (Managed)
import qualified Data.Text as T (pack)
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString, encodeString)
import System.Directory (getHomeDirectory, getTemporaryDirectory)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.Process.Streaming (execute, exitCode, piped, proc)
import qualified Turtle.Prelude as Turtle (mktempdir)

import Game.FMAssistant.Types (Version(..), UserDirFilePath(..))

-- | Construct the default 'UserDirPath' for a given game version.
defaultUserDir :: (MonadIO m) => Version -> m UserDirFilePath
defaultUserDir version =
  do steamDir <- defaultSteamDir
     return $ UserDirFilePath (steamDir </> _version version)

-- | A wrapper around 'Turtle.mktempdir' which always uses the system
-- temporary directory as the parent.
mktempdir :: FilePath -> Managed FilePath
mktempdir template =
  do sysTmpDir <- liftIO $ Filesystem.decodeString <$> getTemporaryDirectory
     Filesystem.encodeString <$> Turtle.mktempdir sysTmpDir (T.pack template)

-- | Execute an external program and return the 'ExitCode'. Both
-- stderr and stdout are discarded.
executeQuietly
        :: (MonadIO m)
        => String
        -- ^ Command
        -> [String]
        -- ^ Arguments
        -> m ExitCode
        -- ^ Exit code
executeQuietly cmd args = liftIO $ execute (piped (proc cmd args)) exitCode

-- Not currently exported.
--

-- | The platform-specific Steam directory for the user who is running
-- the 'MonadIO' action.
defaultSteamDir :: (MonadIO m) => m FilePath
defaultSteamDir =
  do homeDir <- liftIO getHomeDirectory
     return $ homeDir </> "Documents" </> "Sports Interactive"
