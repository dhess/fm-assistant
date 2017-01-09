{-|
Module      : Game.FMAssistant.Repack.Skins
Description : Repack skins
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.Skins
       ( repackSkin
         -- * Exceptions
       , SkinException(..)
       ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir, parseRelDir)
import Path.IO (ensureDir, listDir, renameDir, withSystemTempDir)

import Game.FMAssistant.Mod
       (PackAction(CreateUserDir), packDir, pack)
import Game.FMAssistant.Repack.Internal
       (ArchiveFilePath(..), Repack, RepackException(..), archiveName,
        generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

skinSubDir :: Path Rel Dir
skinSubDir = $(mkRelDir "skins")

repackSkin :: (MonadMask m, MonadIO m) => Repack m
repackSkin archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \tmpDir ->
    do unpackedSkinDir <- unpackSkin archive tmpDir
       withSystemTempDir "repackSkin" $ \tarDir ->
         let modParentDir = tarDir </> packDir CreateUserDir </> skinSubDir
         in do ensureDir modParentDir
               modDir <- parseRelDir $ archiveName archive
               renameDir unpackedSkinDir (modParentDir </> modDir)
               modId <- generateModId archive
               pack tarDir destDir modId

-- | Unpack an archive file assumed to contain a skin to the given
-- parent directory.
--
-- If there is some problem during the unpacking of the archive, or if
-- the skin does not appear to be valid, this action will throw an
-- exception. See the 'SkinException' and 'RepackException' types
-- (although other exceptions are possible, of course, as this action
-- runs in 'MonadIO'.).
unpackSkin :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m (Path Abs Dir)
unpackSkin archive unpackDir =
  do unpack archive unpackDir
     -- Detect malformed packs; put everything under one top-level
     -- directory, if necessary.
     listDir unpackDir >>= \case
       ([], []) -> throwM $ EmptyArchive archive
       ([], [_]) -> throwM $ SingleFileArchive archive
       ([dir], []) -> return dir
       _ ->
         -- Multiple directories at the top level. Supporting these
         -- archives requires some knowledge of what's inside. We
         -- don't currently do that.
         throwM $ UnsupportedSkin archive

-- | Exceptions specific to skin packs.

data SkinException
  = UnsupportedSkin ArchiveFilePath
    -- ^ The skin is unsupported or malformed. Only "simple" skins are
    -- supported, i.e., those where the skin pack contents are
    -- contained in a single subdirectory.
  deriving (Eq,Typeable)

instance Show SkinException where
  show (UnsupportedSkin fp) = show fp ++ ": unsupported skin pack (multiple subdirectories)"

instance Exception SkinException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
