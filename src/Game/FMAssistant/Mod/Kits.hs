{-|
Module      : Game.FMAssistant.Mod.Kits
Description : Types and functions for dealing with kits
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Game.FMAssistant.Mod.Kits
       ( -- * Kit pack types
         KitPath
         -- * Kit pack functions and combinators
       , kitPath
       , filePath
       , installKit
       ) where

import Prelude hiding (FilePath)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Filesystem.Path.CurrentOS ((</>), FilePath)

import Game.FMAssistant.Types (ArchiveFilePath(..), UserDirFilePath(..))

-- | Paths to kits.
--
-- Note that, for type safety, you cannot construct a 'KitPath'
-- directly and must use the 'kitPath' constructor. As kits are always
-- stored in a particular subdirectory of a 'UserDirFilePath', this is
-- not a problem.
newtype KitPath =
  KitPath FilePath
  deriving (Show,Eq,Ord,Data,Typeable)

-- | Construct a kit path.
kitPath :: UserDirFilePath -> KitPath
kitPath ufp =
  KitPath $ _userDirFilePath ufp </> "graphics" </> "kits"

-- | Retrieve the 'FilePath' from a 'KitPath'.
filePath :: KitPath -> FilePath
filePath (KitPath fp) = fp

-- | Install a kit to the given kit path.
installKit :: (MonadIO m) => ArchiveFilePath -> KitPath -> m ()
installKit = undefined
