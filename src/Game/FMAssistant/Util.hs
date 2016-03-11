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
       , createTempDirectory
       , executeQuietly
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import System.Directory (getHomeDirectory, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import qualified System.IO.Temp as System (createTempDirectory)
import System.Process.Streaming (execute, exitCode, piped, proc)

import Game.FMAssistant.Types (Version(..), UserDirFilePath(..))

-- | Construct the default 'UserDirPath' for a given game version.
defaultUserDir :: (MonadIO m) => Version -> m UserDirFilePath
defaultUserDir version =
  do steamDir <- defaultSteamDir
     return $ UserDirFilePath (steamDir </> _version version)

-- | Create a temporary directory and register it with 'ResourceT' so
-- that it's cleaned up properly.
createTempDirectory
        :: (MonadResource m)
        => String
        -- ^ Temp directory filename template
        -> m (ReleaseKey, FilePath)
        -- ^ The 'ReleaseKey' and name of the temporary directory
createTempDirectory template =
  do tmpDir <- liftIO getTemporaryDirectory
     allocate (System.createTempDirectory tmpDir template)
              (\tmp -> removeDirectoryRecursive tmp)

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
