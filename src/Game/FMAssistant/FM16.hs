{-|
Module      : Game.FMAssistant.FM16
Description : FM16-specific functions.
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Game.FMAssistant.FM16
       ( -- * Paths
         defaultUserDir
         -- * Other FM16-specific data
       , version
       ) where

import Prelude hiding (FilePath)
import Control.Monad.IO.Class (MonadIO)

import Game.FMAssistant.Types (Version(..), UserDirFilePath(..))
import qualified Game.FMAssistant.Util as Util (defaultUserDir)

-- | The game version.
--
-- >>> version
-- Football Manager 2016
version :: Version
version = Version "Football Manager 2016"

-- | The default FM16 user directory, where save games, kits, and some
-- other mods go.
defaultUserDir :: (MonadIO m) => m UserDirFilePath
defaultUserDir = Util.defaultUserDir version
