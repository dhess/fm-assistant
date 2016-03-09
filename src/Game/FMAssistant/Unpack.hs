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
import qualified Filesystem.Path.CurrentOS as Filesystem (basename, extension)
import Turtle (ExitCode)
import Turtle.Format (format, fp)
import Turtle.Prelude (ProcFailed(..), procs)

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
unpackerFor :: ArchiveFilePath -> Maybe (ArchiveFilePath -> Managed FilePath)
unpackerFor (ArchiveFilePath ar)
  | Filesystem.extension ar == Just "zip" = Just unpackZip
  | Filesystem.extension ar == Just "ZIP" = Just unpackZip
  | Filesystem.extension ar == Just "rar" = Just unpackRar
  | Filesystem.extension ar == Just "RAR" = Just unpackRar
  | otherwise = Nothing

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
  case unpackerFor ar of
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
unpackZip (ArchiveFilePath zipFile) =
  do tmpDir <- mktempdir (format fp $ Filesystem.basename zipFile)
     runUnpack "unzip" [format fp zipFile, "-d", format fp tmpDir]
     return tmpDir

-- | Unpack a RAR archive to a temporary directory, whose path is
-- returned.
--
-- N.B. that the temporary directory is 'Managed', so it will be
-- automatically removed when the managed action terminates!
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackRar :: ArchiveFilePath -> Managed FilePath
unpackRar (ArchiveFilePath rarFile) =
  do tmpDir <- mktempdir (format fp $ Filesystem.basename rarFile)
     runUnpack "unrar" ["x", "-v", "-y", "-r", format fp rarFile,  format fp tmpDir]
     return tmpDir

-- | Wraps 'procs' with a handler which re-throws 'ProcFailed' as
-- 'UnpackException', for nicer error reporting.
runUnpack :: (MonadIO m) => Text -> [Text] -> m ()
runUnpack cmd args =
  liftIO $ catch (procs cmd args empty) (throwM . toUnpackingError)

data UnpackException
  = UnsupportedArchive ArchiveFilePath -- ^ The archive file type is unsupported
  | UnpackingError Text ExitCode       -- ^ The unpacking command
                                       -- failed; failed command line
                                       -- and process exit code are
                                       -- provided
  deriving (Show,Eq,Typeable)

instance Exception UnpackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException

toUnpackingError :: ProcFailed -> UnpackException
toUnpackingError (ProcFailed cmd args exitCode) =
  UnpackingError (cmd <> T.intercalate " " args) exitCode
