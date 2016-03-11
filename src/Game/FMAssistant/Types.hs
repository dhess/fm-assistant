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
{-# LANGUAGE Safe #-}

module Game.FMAssistant.Types
       ( -- * Types
         Version(..)
       , UserDirFilePath(..)
       , ArchiveFilePath(..)
       , archiveName
         -- * Exceptions
       , SomeFMAssistantException
       , fmAssistantExceptionToException
       , fmAssistantExceptionFromException
       ) where

import Control.Exception (Exception, SomeException, fromException, toException)
import Data.Data
import System.FilePath (takeBaseName)

-- | Game's name and version, e.g., "Football Manager 2016". This is
-- often used as the component of a pathname.
newtype Version =
  Version {_version :: FilePath}
  deriving (Eq,Ord,Data,Typeable)

instance Show Version where
  show = _version

-- | The type for paths which point to a user directory, where game
-- saves, kits, and most other mods, are stored.
newtype UserDirFilePath =
  UserDirFilePath {_userDirFilePath :: FilePath}
  deriving (Eq,Ord,Data,Typeable)

instance Show UserDirFilePath where
  show = _userDirFilePath

-- | The type for paths which point to an archive file (ZIP, RAR,
-- etc.).
newtype ArchiveFilePath =
  ArchiveFilePath {_archiveFilePath :: FilePath}
  deriving (Eq,Ord,Data,Typeable)

instance Show ArchiveFilePath where
  show = _archiveFilePath

-- | Return the name of an archive file; that is, given an
-- 'ArchiveFilePath', strip off the pathname portion and the trailing
-- file suffix, and return what remains.
--
-- >>> :set -XOverloadedStrings
-- >>> archiveName $ ArchiveFilePath "/foo/bar/baz.rar"
-- "baz"
archiveName :: ArchiveFilePath -> FilePath
archiveName = takeBaseName . _archiveFilePath

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
