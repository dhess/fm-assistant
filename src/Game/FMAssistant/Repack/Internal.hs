{-|
Module      : Game.FMAssistant.Repack.Internal
Description : Internal utilities for repacking mods
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.Internal
       ( generateModId
       ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock (utctDay)
import Path.IO (getModificationTime)

import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Util (basename)

generateModId :: (MonadIO m, MonadThrow m) => ArchiveFilePath -> m String
generateModId (ArchiveFilePath archive) =
  let base = basename archive
  in do modifiedTime <- getModificationTime archive
        return $ base ++ "." ++ showGregorian (utctDay modifiedTime)
