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

import Prelude hiding (FilePath)
import Control.Applicative (empty)
import Control.Exception (Exception(..), catch, throw)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (Managed)
import Data.Data
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (intercalate)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (basename)
import Turtle (ExitCode)
import Turtle.Format (format, fp)
import Turtle.Prelude (ProcFailed(..), procs)

import Game.FMAssistant.Magic (Magic(..), identifyArchive)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), fmAssistantExceptionToException,
        fmAssistantExceptionFromException)
import Game.FMAssistant.Util (mktempdir)

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
  do tmpDir <- mktempdir (format fp $ Filesystem.basename zipFile)
     liftIO $
       catch (procs "unzip" [format fp zipFile, "-d", format fp tmpDir] empty)  (throwM . toUnpackingError)
     return tmpDir
  where
    toUnpackingError :: ProcFailed -> UnpackException
    toUnpackingError (ProcFailed cmd args exitCode) =
      UnzipError ar (cmd <> T.intercalate " " args ) exitCode

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
  do tmpDir <- mktempdir (format fp $ Filesystem.basename rarFile)
     liftIO $
       catch (procs "unrar" ["x", "-v", "-y", "-r", format fp rarFile,  format fp tmpDir] empty) (throwM . toUnpackingError)
     return tmpDir
  where
    toUnpackingError :: ProcFailed -> UnpackException
    toUnpackingError (ProcFailed cmd args exitCode) =
      UnrarError ar (cmd <> T.intercalate " " args ) exitCode

data UnpackException
  = UnsupportedArchive ArchiveFilePath -- ^ The archive file type is unsupported
  | UnzipError ArchiveFilePath Text ExitCode -- ^ The unzip command
                                             -- failed; failed command
                                             -- line and process exit
                                             -- code are provided
  | UnrarError ArchiveFilePath Text ExitCode -- ^ The unrar command
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
