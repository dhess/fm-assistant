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

import Control.Exception (Exception(..), throw)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Managed.Safe (Managed)
import Data.Data
import Data.Monoid ((<>))
import System.Exit (ExitCode(..))
import System.FilePath (takeBaseName)

import Game.FMAssistant.Magic (Magic(..), identifyArchive)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), fmAssistantExceptionToException,
        fmAssistantExceptionFromException)
import Game.FMAssistant.Util (executeQuietly, mktempdir)

-- | Given the filename of an archive file, use the filename's
-- extension to guess which unpack action to use, and return that
-- action.
--
-- If, based on the filename's extension, the archive format is
-- unsupported, this function returns 'Nothing'.
unpackerFor :: (MonadIO m) => ArchiveFilePath -> m (Maybe (ArchiveFilePath -> Managed FilePath))
unpackerFor ar =
  identifyArchive ar >>= \case
    Just Zip -> return $ Just unpackZip
    Just Rar -> return $ Just unpackRar
    Nothing -> return Nothing

-- | Attempt to guess the format of the specified archive file and
-- unpack it to a temporary directory, whose path is returned.
--
-- N.B. that the temporary directory is 'Managed', so it will be
-- automatically removed when the managed action terminates!
--
-- If the archive file format cannot be guessed or is not supported,
-- this action throws an 'UnpackException' with a value of
-- 'UnsupportedArchive'.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpack :: ArchiveFilePath -> Managed FilePath
unpack ar =
  unpackerFor ar >>= \case
    Just unpacker -> unpacker ar
    Nothing -> throw $ UnsupportedArchive ar

-- | Unpack a ZIP archive to a temporary directory, whose path is
-- returned.
--
-- N.B. that the temporary directory is 'Managed', so it will be
-- automatically removed when the managed action terminates!
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackZip :: ArchiveFilePath -> Managed FilePath
unpackZip ar@(ArchiveFilePath zipFile) =
  let unzipCmd :: FilePath
      unzipCmd = "unzip"
  in
    do tmpDir <- mktempdir (takeBaseName zipFile)
       let args = [zipFile, "-d", tmpDir]
       exitCode <- executeQuietly unzipCmd args
       if exitCode /= ExitSuccess
          then throw $ UnzipError ar (unzipCmd <> unwords args ) exitCode
          else return tmpDir

-- | Unpack a RAR archive to a temporary directory, whose path is
-- returned.
--
-- N.B. that the temporary directory is 'Managed', so it will be
-- automatically removed when the managed action terminates!
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackRar :: ArchiveFilePath -> Managed FilePath
unpackRar ar@(ArchiveFilePath rarFile) =
  let unrarCmd :: FilePath
      unrarCmd = "unrar"
  in
    do tmpDir <- mktempdir (takeBaseName rarFile)
       let args = ["x", "-v", "-y", "-r", rarFile,  tmpDir]
       exitCode <- executeQuietly unrarCmd args
       if exitCode /= ExitSuccess
          then throw $ UnrarError ar (unrarCmd <> unwords args ) exitCode
          else return tmpDir

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
