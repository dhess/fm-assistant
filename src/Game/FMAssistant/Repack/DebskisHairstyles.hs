{-|
Module      : Game.FMAssistant.Repack.DebskisHairstyles
Description : Repack the Metallic Logos mod
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.DebskisHairstyles
       ( repackDebskisHairstyles
         -- * Exceptions
       , DebskisHairstylesRepackException(..)
       ) where

import Control.Conditional (unlessM)
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path ((</>), Path, Rel, Dir, mkRelDir, parent)
import Path.IO
       (doesDirExist, ensureDir, renameDir, withSystemTempDir)

import Game.FMAssistant.Mod
       (PackAction(CreateAppDir), packDir, pack)
import Game.FMAssistant.Repack.Internal
       (ArchiveFilePath(..), Repack, generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

modHairDir :: Path Rel Dir
modHairDir = $(mkRelDir "Debski's World of Hairstyles for FM16 v1.0")

gameHairDir :: Path Rel Dir
gameHairDir = $(mkRelDir "data/facegen/hair")

-- | Repack Debski's World of Hairstyles pack.
repackDebskisHairstyles :: (MonadThrow m, MonadMask m, MonadIO m) => Repack m
repackDebskisHairstyles archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \unpackDir ->
    do unpack archive unpackDir
       let unpackedHairDir = unpackDir </> modHairDir
       unlessM (doesDirExist unpackedHairDir) $
         throwM $ MissingHairDir archive
       withSystemTempDir "repackDebskisHairstyles" $ \tarDir ->
         let packHairDir = tarDir </> packDir CreateAppDir </> gameHairDir </> modHairDir
         in do ensureDir (parent packHairDir)
               renameDir unpackedHairDir packHairDir
               modId <- generateModId archive
               pack tarDir destDir modId

data DebskisHairstylesRepackException
  = MissingHairDir ArchiveFilePath
    -- ^ The archive is missing the hair directory
  deriving (Eq,Typeable)

instance Show DebskisHairstylesRepackException where
  show (MissingHairDir fp) = show fp ++ ": Malformed hair pack (no hair directory)"

instance Exception DebskisHairstylesRepackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
