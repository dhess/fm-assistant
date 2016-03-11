{-|
Module      : Game.FMAssistant.Unpack
Description : Utilities for unpacking archives
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Functions for unpacking the various archive types (RAR, ZIP, etc.).

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Game.FMAssistant.Unpack
       ( -- * Unpacking actions
         unpackZip
       , unpackRar
       , unpack
         -- * Selecting an unpacking action
       , unpackerFor
         -- * Unpacking-related exceptions
       , UnpackException(..)
       ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import Data.Data
import Data.Monoid ((<>))
import System.Exit (ExitCode(..))
import System.FilePath (takeBaseName)

import Game.FMAssistant.Magic (Magic(..), identifyArchive)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), fmAssistantExceptionToException,
        fmAssistantExceptionFromException)
import Game.FMAssistant.Util (executeQuietly, createTempDirectory)

-- | Given the filename of an archive file, use the filename's
-- extension to guess which unpack action to use, and return that
-- action.
--
-- If, based on the filename's extension, the archive format is
-- unsupported, this function returns 'Nothing'.
unpackerFor :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> m (Maybe (ArchiveFilePath -> FilePath -> m ()))
unpackerFor ar =
  identifyArchive ar >>= \case
    Just Zip -> return $ Just unpackZip
    Just Rar -> return $ Just unpackRar
    Nothing -> return Nothing

-- | Attempt to guess the format of the specified archive file and
-- unpack it to a temporary directory, whose path and 'ReleaseKey' are
-- returned.
--
-- If the archive file format cannot be guessed or is not supported,
-- this action throws an 'UnpackException' with a value of
-- 'UnsupportedArchive'.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpack :: (MonadResource m) => ArchiveFilePath -> m (ReleaseKey, FilePath)
unpack ar@(ArchiveFilePath fn) =
  unpackerFor ar >>= \case
    Nothing -> throwM $ UnsupportedArchive ar
    Just unpacker ->
      do resource@(_, tmpDir) <- createTempDirectory (takeBaseName fn)
         unpacker ar tmpDir
         return resource

-- | Unpack a ZIP archive to the given directory.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackZip :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> FilePath -> m ()
unpackZip ar@(ArchiveFilePath zipFile) unpackDir =
  let unzipCmd :: FilePath
      unzipCmd = "unzip"
      args :: [String]
      args = [zipFile, "-d", unpackDir]
  in
    do exitCode <- executeQuietly unzipCmd args
       if exitCode == ExitSuccess
          then return ()
          else throwM $ UnzipError ar (unzipCmd <> unwords args ) exitCode

-- | Unpack a RAR archive to the given directory.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackRar :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> FilePath -> m ()
unpackRar ar@(ArchiveFilePath rarFile) unpackDir =
  let unrarCmd :: FilePath
      unrarCmd = "unrar"
      args :: [String]
      args = ["x", "-v", "-y", "-r", rarFile, unpackDir]
  in
    do exitCode <- executeQuietly unrarCmd args
       if exitCode == ExitSuccess
          then return ()
          else throwM $ UnrarError ar (unrarCmd <> unwords args ) exitCode

data UnpackException
  = UnsupportedArchive ArchiveFilePath -- ^ The archive file type is unsupported
  | UnzipError ArchiveFilePath String ExitCode -- ^ The unzip command
                                                   -- failed; failed command
                                                   -- line and process exit
                                                   -- code are provided
  | UnrarError ArchiveFilePath String ExitCode -- ^ The unrar command
                                                   -- failed; failed command
                                                   -- line and process exit
                                                   -- code are provided
  deriving (Eq,Typeable)

instance Show UnpackException where
  show (UnsupportedArchive ar) = show ar ++ ": Unsupported archive type"
  show (UnzipError ar _ exitCode) = show ar ++ ": unzip command failed, make sure it's in your path and check the file (exit code " ++ show exitCode ++ ")"
  show (UnrarError ar _ exitCode) = show ar ++ ": unrar command failed, make sure it's in your path and check the file (exit code " ++ show exitCode ++ ")"

instance Exception UnpackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
