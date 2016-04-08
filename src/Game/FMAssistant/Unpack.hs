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
{-# LANGUAGE Trustworthy #-}

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
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Data.Monoid ((<>))
import Path (toFilePath)
import System.Exit (ExitCode(..))

import Game.FMAssistant.Magic (Magic(..), identifyArchive)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), UnpackDirPath(..),
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (executeQuietly)

-- | Given the filename of an archive file, use the filename's
-- extension to guess which unpack action to use, and return that
-- action.
--
-- If, based on the filename's extension, the archive format is
-- unsupported, this function returns 'Nothing'.
unpackerFor :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> m (Maybe (ArchiveFilePath -> UnpackDirPath -> m ()))
unpackerFor ar =
  identifyArchive ar >>= \case
    Just Zip -> return $ Just unpackZip
    Just Rar -> return $ Just unpackRar
    Nothing -> return Nothing

-- | Attempt to guess the format of the specified archive file and
-- unpack it to the given directory.
--
-- If the archive file format cannot be guessed or is not supported,
-- this action throws an 'UnpackException' with a value of
-- 'UnsupportedArchive'.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpack :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> UnpackDirPath -> m ()
unpack archive unpackDir =
  unpackerFor archive >>= \case
    Nothing -> throwM $ UnsupportedArchive archive
    Just unpacker -> unpacker archive unpackDir

-- | Unpack a ZIP archive to the given directory.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackZip :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> UnpackDirPath -> m ()
unpackZip (ArchiveFilePath zipFile) (UnpackDirPath unpackDir) =
  let unzipCmd :: FilePath
      unzipCmd = "unzip"
      args :: [String]
      args = [toFilePath zipFile, "-d", toFilePath unpackDir]
  in
    do exitCode <- executeQuietly unzipCmd args
       unless (exitCode == ExitSuccess) $
         throwM $ UnzipError (unzipCmd <> unwords args) exitCode

-- | Unpack a RAR archive to the given directory.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackRar :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> UnpackDirPath -> m ()
unpackRar (ArchiveFilePath rarFile) (UnpackDirPath unpackDir) =
  let unrarCmd :: FilePath
      unrarCmd = "unrar"
      args :: [String]
      args = ["x", "-v", "-y", "-r", toFilePath rarFile, toFilePath unpackDir]
  in
    do exitCode <- executeQuietly unrarCmd args
       unless (exitCode == ExitSuccess) $
         throwM $ UnrarError (unrarCmd <> unwords args) exitCode

data UnpackException
  = UnsupportedArchive ArchiveFilePath
    -- ^ The archive format is unsupported
  | UnzipError String ExitCode
    -- ^ The unzip command failed; failed command line and process
    -- exit code are provided
  | UnrarError String ExitCode
    -- ^ The unrar command failed; failed command line and process
    -- exit code are provided
  deriving (Eq,Typeable)

instance Show UnpackException where
  show (UnsupportedArchive ar) = "Unsupported archive type " ++ show ar
  show (UnzipError _ exitCode) = "unzip command failed, make sure it's in your path and check the archive (exit code " ++ show exitCode ++ ")"
  show (UnrarError _ exitCode) = "unrar command failed, make sure it's in your path and check the archive (exit code " ++ show exitCode ++ ")"

instance Exception UnpackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
