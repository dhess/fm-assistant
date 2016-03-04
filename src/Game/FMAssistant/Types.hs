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
{-# LANGUAGE Safe #-}

module Game.FMAssistant.Types
       ( Version(..)
       , versionToFilePath
       , UserDirFilePath(..)
       ) where

import Prelude hiding (FilePath)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Data
import Data.String (IsString(..))
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (fromText)

-- | Game's name and version, e.g., "Football Manager 2016". This is
-- often used as the component of a pathname.
newtype Version =
  Version {_version :: Text}
  deriving (Show,Eq,Ord,Data,Typeable)

instance IsString Version where
  fromString = Version . T.pack

-- | Convert a 'Version' value to a 'FilePath'.
versionToFilePath :: Version -> FilePath
versionToFilePath = Filesystem.fromText . _version

-- | The type for paths which point to a user directory, where game
-- saves, kits, and most other mods, are stored.
newtype UserDirFilePath =
  UserDirFilePath {_userDirFilePath :: FilePath}
  deriving (Show,Eq,Ord,Data,Typeable)
