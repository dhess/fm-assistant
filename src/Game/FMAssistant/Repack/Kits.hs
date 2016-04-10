{-|
Module      : Game.FMAssistant.Repack.Kits
Description : Repack kit packs
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.Kits
       ( repackKitPack
       ) where

import Control.Exception (Exception(..))
import Control.Monad (forM_)
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path ((</>), Path, Abs, Rel, Dir, dirname, filename, mkRelDir, parseRelDir)
import Path.IO
       (createDir, ensureDir, listDir, renameDir, renameFile,
        withSystemTempDir)

import Game.FMAssistant.Mod
       (PackFilePath, PackAction(CreateUserDir), packDir, packMod)
import Game.FMAssistant.Repack.Internal (generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), UnpackDirPath(..), archiveName,
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

kitSubDir :: Path Rel Dir
kitSubDir = $(mkRelDir "graphics/kits")

repackKitPack :: (MonadMask m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m PackFilePath
repackKitPack archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \tmpDir ->
    do unpackedKitDir <- unpackKitPack archive (UnpackDirPath tmpDir)
       withSystemTempDir "repackKitPack" $ \tarDir ->
         let modParentDir = tarDir </> packDir CreateUserDir </> kitSubDir
         in do ensureDir modParentDir
               modDir <- parseRelDir $ archiveName archive
               renameDir (unpackDirPath unpackedKitDir) (modParentDir </> modDir)
               modId <- generateModId archive
               packMod tarDir destDir modId

-- | Unpack an archive file assumed to contain a kit pack to the given
-- parent directory.
--
-- If there is some problem during the unpacking of the archive, or if
-- the kit pack does not appear to be valid, this action will throw an
-- exception. See the 'KitPackException' type for exceptions specific
-- to kit packs (although other exceptions are possible, of course, as
-- this action runs in 'MonadIO'.).
unpackKitPack :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> UnpackDirPath -> m UnpackDirPath
unpackKitPack archive unpackDir =
  do unpack archive unpackDir
     -- Detect malformed packs; put everything under one top-level
     -- directory, if necessary.
     listDir (unpackDirPath unpackDir) >>= \case
       ([], []) -> throwM $ EmptyArchive archive
       ([], [_]) -> throwM $ SingleFileArchive archive
       ([dir], []) -> return $ UnpackDirPath dir
       (dirs, files) ->
         do fixdir <- parseRelDir $ archiveName archive
            let fixpath = unpackDirPath unpackDir </> fixdir
            createDir fixpath
            forM_ dirs $ \dn ->
              renameDir dn (fixpath </> dirname dn)
            forM_ files $ \fn ->
              renameFile fn (fixpath </> filename fn)
            return $ UnpackDirPath fixpath

data KitPackException
  = EmptyArchive ArchiveFilePath
    -- ^ The archive is empty
  | SingleFileArchive ArchiveFilePath
    -- ^ The archive contains just a single file and can't be a valid
    -- kit pack
  deriving (Eq,Typeable)

instance Show KitPackException where
  show (EmptyArchive fp) = show fp ++ ": Malformed kit pack (archive file is empty)"
  show (SingleFileArchive fp) = show fp ++ ": Malformed kit pack (archive file contains just a single file)"

instance Exception KitPackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
