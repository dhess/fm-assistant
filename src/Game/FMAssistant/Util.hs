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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Util
       ( createSystemTempDir
       , createTempDir
       , executeQuietly
       , basename
       , touchFile
       ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Path (Path, Abs, Dir, File, filename, toFilePath)
import Path.IO (doesFileExist, getTempDir, removeDirRecur)
import qualified Path.IO as Path (createTempDir)
import System.Exit (ExitCode)
import System.FilePath (takeBaseName)
import System.IO (IOMode(WriteMode), withFile)
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
  do tmpDir <- getTempDir
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
  allocate (Path.createTempDir parentDir template) removeDirRecur

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

-- | Return (as a 'String') the filename of the given file path,
-- without the extension.
basename
  :: Path b File
  -- ^ The file path
  -> String
  -- ^ The filename without the extension
basename = takeBaseName . toFilePath . filename

-- | Create a zero-length file, if it doesn't already exist;
-- otherwise, do nothing.
touchFile :: (MonadIO m) => Path Abs File -> m ()
touchFile file =
  do exists <- doesFileExist file
     unless exists $
       liftIO $ withFile (toFilePath file) WriteMode (\_ -> return ())
