{-|
Module      : Game.FMAssistant.Repack.Unpack
Description : Utilities for unpacking archives
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Actions for unpacking the various archive types (RAR, ZIP, etc.).

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.Unpack
       ( -- * Unpacking actions
         unpackZip
       , unpackRar
       , unpack
         -- * Unpacking-related exceptions
       , UnpackException(..)
       ) where

import Control.Conditional (whenM)
import Control.Exception (Exception(..))
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Data.Monoid ((<>))
import Path ((</>), Path, Abs, Dir, mkRelDir, toFilePath)
import Path.IO (doesDirExist, removeDirRecur)
import System.Exit (ExitCode(..))

import Game.FMAssistant.Magic (Magic(..), identifyFile)
import Game.FMAssistant.Repack.Internal (ArchiveFilePath(..))
import Game.FMAssistant.Types
       (fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (executeQuietly)

-- | Attempt to guess the format of the specified archive file and
-- unpack it to the given directory.
--
-- If the archive file format cannot be guessed or is not supported,
-- this action throws an 'UnpackException' with a value of
-- 'UnsupportedArchive'.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpack :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m ()
unpack archive unpackDir =
  identifyFile (archiveFilePath archive) >>= \case
    Just Zip -> unpackZip archive unpackDir
    Just Rar -> unpackRar archive unpackDir
    Nothing -> throwM $ UnsupportedArchive archive

data Cmd
  = Unrar
  | Unzip
  deriving (Eq)

-- | Unpack a ZIP archive to the given directory.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackZip :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m ()
unpackZip zipFile unpackDir =
  let args :: [String]
      args = [toFilePath (archiveFilePath zipFile), "-d", toFilePath unpackDir]
  in do unzipAction Unzip args
        osxFixUp unpackDir

-- | Unpack a RAR archive to the given directory.
--
-- If there's a problem unpacking the archive file, this action throws
-- an 'UnpackException' with a value of 'UnpackingError'.
unpackRar :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m ()
unpackRar rarFile unpackDir =
  let args :: [String]
      args = ["x", "-v", "-y", "-r", toFilePath (archiveFilePath rarFile), toFilePath unpackDir]
  in do unzipAction Unrar args
        osxFixUp unpackDir

unzipAction :: (MonadThrow m, MonadIO m) => Cmd -> [String] -> m ()
unzipAction cmd args =
  do exitCode <- executeQuietly (bin cmd) args
     unless (exitCode == ExitSuccess) $
       throwM $ failure cmd (bin cmd <> unwords args) exitCode
  where
    bin :: Cmd -> FilePath
    bin Unrar = "unrar"
    bin Unzip = "unzip"
    failure :: Cmd -> String -> ExitCode -> UnpackException
    failure Unrar = UnrarError
    failure Unzip = UnzipError

-- | Remove any top-level "__MACOSX" subdirectory.
osxFixUp :: (MonadIO m) => Path Abs Dir -> m ()
osxFixUp unpackDir =
  let osxdir :: Path Abs Dir
      osxdir = unpackDir </> $(mkRelDir "__MACOSX")
  in whenM (doesDirExist osxdir) $
       removeDirRecur osxdir

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
