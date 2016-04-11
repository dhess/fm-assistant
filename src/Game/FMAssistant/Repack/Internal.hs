{-|
Module      : Game.FMAssistant.Repack.Internal
Description : Internal utilities for repacking mods
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.Internal
       ( -- * File paths
         ArchiveFilePath(..)
       , archiveName
         -- * Utilities
       , generateModId
         -- * Exceptions
       , RepackException(..)
       ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock (utctDay)
import Path (Path, Abs, File, parseAbsFile)
import Path.IO (getModificationTime)

import Game.FMAssistant.Types
       (fmAssistantExceptionToException,
        fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

-- | The type for paths which point to an archive file (ZIP, RAR,
-- etc.).
newtype ArchiveFilePath =
  ArchiveFilePath {archiveFilePath :: Path Abs File}
  deriving (Eq,Show,Ord,Typeable)

-- | Return (as a 'String') the name of an archive file; that is,
-- given an 'ArchiveFilePath', strip off the pathname portion and the
-- trailing file suffix, and return what remains.
--
-- >>> :set -XOverloadedStrings
-- >>> bazPath <- parseAbsFile "/foo/bar/baz.rar"
-- >>> archiveName $ ArchiveFilePath bazPath
-- "baz"
archiveName :: ArchiveFilePath -> String
archiveName = basename . archiveFilePath

generateModId :: (MonadIO m, MonadThrow m) => ArchiveFilePath -> m String
generateModId (ArchiveFilePath archive) =
  let base = basename archive
  in do modifiedTime <- getModificationTime archive
        return $ base ++ "." ++ showGregorian (utctDay modifiedTime)

data RepackException
  = EmptyArchive ArchiveFilePath
    -- ^ The archive is empty
  | SingleFileArchive ArchiveFilePath
    -- ^ The archive contains just a single file and can't be a valid
    -- kit pack
  deriving (Eq,Typeable)

instance Show RepackException where
  show (EmptyArchive fp) = show fp ++ ": Malformed pack (archive file is empty)"
  show (SingleFileArchive fp) = show fp ++ ": Malformed pack (archive file contains just a single file)"

instance Exception RepackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
