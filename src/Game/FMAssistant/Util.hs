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
       ( createSystemTempDir
       , createSystemTempDirectory
       , createTempDir
       , defaultSteamDir
       , listDirectory
       , executeQuietly
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Path (Path, Abs, Dir)
import qualified Path.IO as Path (createTempDir, getTempDir, removeDirRecur)
import System.Directory (getHomeDirectory, getDirectoryContents, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode)
import System.FilePath ((</>), combine)
import qualified System.IO.Temp as System (createTempDirectory)
import System.Process.Streaming (execute, exitCode, piped, proc)

-- | Create a temporary directory in the system temporary directory
-- and register it with 'ResourceT' so that it's cleaned up properly.
createSystemTempDir
  :: (MonadResource m)
  => String
  -- ^ Temp directory filename template
  -> m (ReleaseKey, Path Abs Dir)
  -- ^ The 'ReleaseKey' and name of the temporary directory
createSystemTempDir template =
  do tmpDir <- Path.getTempDir
     createTempDir tmpDir template

-- | Create a temporary directory in the given parent directory and
-- register it with 'ResourceT' so that it's cleaned up properly.
createTempDir
  :: (MonadResource m)
  => Path Abs Dir
  -- ^ The parent directory in which to create the temporary directory
  -> String
  -- ^ Temp directory filename template
  -> m (ReleaseKey, Path Abs Dir)
  -- ^ The 'ReleaseKey' and name of the temporary directory
createTempDir parentDir template =
  allocate (Path.createTempDir parentDir template) Path.removeDirRecur

-- | Like 'createSystemTempDir' but with standard Haskell 'FilePath's.
createSystemTempDirectory
        :: (MonadResource m)
        => String
        -- ^ Temp directory filename template
        -> m (ReleaseKey, FilePath)
        -- ^ The 'ReleaseKey' and name of the temporary directory
createSystemTempDirectory template =
  do tmpDir <- liftIO getTemporaryDirectory
     allocate (System.createTempDirectory tmpDir template)
              removeDirectoryRecursive

-- | List a directory, excluding "." and "..". Note that the returned
-- 'FilePath's include the full directory path.
listDirectory :: (MonadIO m) => FilePath -> m [FilePath]
listDirectory path = liftIO $
  do ls <- filter f <$> getDirectoryContents path
     return $ map (combine path) ls
  where f filename = filename /= "." && filename /= ".."

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

-- | The platform-specific Steam directory for the user who is running
-- the 'MonadIO' action.
defaultSteamDir :: (MonadIO m) => m FilePath
defaultSteamDir =
  do homeDir <- liftIO getHomeDirectory
     return $ homeDir </> "Documents" </> "Sports Interactive"
