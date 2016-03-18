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
       ( createTempDirectory
       , defaultSteamDir
       , listDirectory
       , executeQuietly
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import System.Directory (getHomeDirectory, getDirectoryContents, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode)
import System.FilePath ((</>), combine)
import qualified System.IO.Temp as System (createTempDirectory)
import System.Process.Streaming (execute, exitCode, piped, proc)

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
