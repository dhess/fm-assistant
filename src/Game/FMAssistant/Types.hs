{-|
Module      : Game.FMAssistant.Types
Description : Types used by the package
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Types
       ( -- * Types
         VersionDirPath(..)
       , UserDirPath(..)
       , ArchiveFilePath(..)
       , UnpackDirPath(..)
       , archiveName
         -- * Exceptions
       , SomeFMAssistantException
       , fmAssistantExceptionToException
       , fmAssistantExceptionFromException
       ) where

import Control.Exception (Exception, SomeException, fromException, toException)
import Data.Data
import Path (Path, Abs, Rel, Dir, File, parseAbsFile)

import Game.FMAssistant.Util (basename)

-- | Game's name and version, e.g., "Football Manager 2016". This is
-- often used as the component of a pathname.
newtype VersionDirPath =
  VersionDirPath {_versionDirPath :: Path Rel Dir}
  deriving (Eq,Show,Ord,Typeable)

-- | The type for paths which point to a user directory, where game
-- saves, kits, and most other mods, are stored.
newtype UserDirPath =
  UserDirPath {_userDirPath :: Path Abs Dir}
  deriving (Eq,Show,Ord,Typeable)

-- | The type for paths which point to an archive file (ZIP, RAR,
-- etc.).
newtype ArchiveFilePath =
  ArchiveFilePath {_archiveFilePath :: Path Abs File}
  deriving (Eq,Show,Ord,Typeable)

-- | The type for paths which point to an unpacked archive directory.
newtype UnpackDirPath =
  UnpackDirPath {_unpackDirPath :: Path Abs Dir}
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
archiveName = basename . _archiveFilePath

-- | The root exception type for @fm-assistant@.
data SomeFMAssistantException =
  forall e. Exception e => SomeFMAssistantException e
  deriving (Typeable)

instance Show SomeFMAssistantException where
  show (SomeFMAssistantException e) = show e

instance Exception SomeFMAssistantException

fmAssistantExceptionToException :: Exception e => e -> SomeException
fmAssistantExceptionToException = toException . SomeFMAssistantException

fmAssistantExceptionFromException :: Exception e => SomeException -> Maybe e
fmAssistantExceptionFromException x =
  do SomeFMAssistantException a <- fromException x
     cast a
