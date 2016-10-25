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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Types
       ( -- * Game versions
         Version(..)
       , versionDir
         -- * Path types
       , AppDirPath(..)
       , defaultAppDir
       , UserDirPath(..)
       , defaultUserDir
       , BackupDirPath(..)
       , defaultBackupDir
         -- * Exceptions
       , SomeFMAssistantException
       , fmAssistantExceptionToException
       , fmAssistantExceptionFromException
       ) where

import Control.Exception (Exception, SomeException, fromException, toException)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir)
import Path.IO (getHomeDir)

-- | The game version.
data Version =
  FM16
  deriving (Eq,Show,Ord,Typeable,Read)

-- | The game version subdirectory.
--
-- >>> versionDir FM16
-- "Football Manager 2016/"
versionDir :: Version -> Path Rel Dir
versionDir FM16 = $(mkRelDir "Football Manager 2016")

-- | Paths which point to application directories, i.e., where the
-- game's files are located.
newtype AppDirPath =
  AppDirPath {appDirPath :: Path Abs Dir}
  deriving (Eq,Show,Ord,Typeable)

-- | The default app directory for a given game version.
defaultAppDir :: (MonadThrow m, MonadIO m) => Version -> m AppDirPath
defaultAppDir version =
  do homeDir <- getHomeDir
     return $
       AppDirPath (homeDir </>
                   $(mkRelDir "Library/Application Support/Steam/SteamApps/common") </>
                   versionDir version)

-- | The type for paths which point to a user directory, where game
-- saves, kits, and most other mods, are stored.
newtype UserDirPath =
  UserDirPath {userDirPath :: Path Abs Dir}
  deriving (Eq,Show,Ord,Typeable)

--- | The default user directory for a given game version, where save
--- games and most mod types live.
defaultUserDir :: (MonadThrow m, MonadIO m) => Version -> m UserDirPath
defaultUserDir version =
  do homeDir <- getHomeDir
     return $
       UserDirPath (homeDir </>
                    $(mkRelDir "Documents/Sports Interactive") </>
                    versionDir version)

-- | Paths for storing backups of game content which mods might
-- overwrite or delete.
newtype BackupDirPath =
  BackupDirPath {backupDirPath :: Path Abs Dir}
  deriving (Eq,Show,Ord,Typeable)

-- | The default backup directory.
defaultBackupDir :: (MonadThrow m, MonadIO m) => Version -> m BackupDirPath
defaultBackupDir version =
  do homeDir <- getHomeDir
     return $
       BackupDirPath (homeDir </>
                     $(mkRelDir "Library/Application Support/FMAssistant") </>
                     versionDir version)

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
