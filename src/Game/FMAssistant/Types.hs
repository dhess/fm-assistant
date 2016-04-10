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
       , UserDirPath(..)
       , defaultUserDir
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

import Game.FMAssistant.Util (defaultSteamDir)

-- | The game version.
data Version =
  FM16
  deriving (Eq,Show,Ord,Typeable)

-- | The game version subdirectory.
--
-- >>> versionDir FM16
-- "Football Manager 2016/"
versionDir :: Version -> Path Rel Dir
versionDir FM16 = $(mkRelDir "Football Manager 2016")

-- | The type for paths which point to a user directory, where game
-- saves, kits, and most other mods, are stored.
newtype UserDirPath =
  UserDirPath {userDirPath :: Path Abs Dir}
  deriving (Eq,Show,Ord,Typeable)

--- | The default user directory, where save games and most mod types
--- live.
defaultUserDir :: (MonadThrow m, MonadIO m) => Version -> m UserDirPath
defaultUserDir version =
  do steamDir <- defaultSteamDir
     return $ UserDirPath (steamDir </> versionDir version)

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
