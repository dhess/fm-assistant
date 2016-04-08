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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Mod.Kits
       ( -- * Kit pack paths
         kitPath
         -- * Install actions
       , installKitPack
       , validateKitPack
         -- * Kit pack-related exceptions
       , KitPackException(..)
       ) where

import Control.Exception (Exception(..))
import Control.Lens
import Control.Monad (forM_, void)
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..))
import Data.Data
import Path ((</>), Path, Abs, Dir, Rel, dirname, filename, mkRelDir, parseAbsDir, parseRelDir)
import Path.IO
       (createDir, ensureDir, listDir, renameDir, renameFile,
        withSystemTempDir)

import Game.FMAssistant.Install (HasInstallConfig(..), install, checkUserDir)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), UserDirPath(..), UnpackDirPath(..),
        archiveName, fmAssistantExceptionToException,
        fmAssistantExceptionFromException)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Util (basename)

-- | Kits live in a pre-determined subdirectory of the game's user
-- directory. This function constructs the path to that subdirectory,
-- given a particular 'UserDirPath'.
--
-- >>> :set -XOverloadedStrings
-- >>> userDir <- parseAbsDir "/home/dhess/Football Manager 2016"
-- >>> kitPath $ UserDirPath userDir
-- "/home/dhess/Football Manager 2016/graphics/kits/"
kitPath :: UserDirPath -> Path Abs Dir
kitPath ufp = userDirPath ufp </> kitSubDir

kitSubDir :: Path Rel Dir
kitSubDir = $(mkRelDir "graphics/kits")

-- | Verify that a kit pack archive file is valid, or can be
-- automatically fixed up so that it's valid.
--
-- If the kit pack is successfully validated, this action simply
-- returns a void result; the kit pack is not actually installed, nor
-- its unpacked contents otherwise kept around.
--
-- If the pack is found to be invalid, this action throws a
-- 'KitPackException'. (As this action runs in 'IO', other exceptions
-- may occur, of course, but they will most likely not be an
-- indication that anything is wrong with the kit pack.)
--
-- It's not necessary to run this action prior to attempting to
-- install the kit pack, as 'installKitPack' will perform the same
-- checks as this action does. This action is provided primarily to
-- help users weed faulty kit packs out of their collections, e.g., by
-- using it as a filter on a directory full of kit pack archives.
validateKitPack :: (MonadThrow m, MonadMask m, MonadIO m) => ArchiveFilePath -> m ()
validateKitPack ar@(ArchiveFilePath fn) =
  withSystemTempDir (basename fn) $ \tmpDir ->
    void $ unpackKitPack ar (UnpackDirPath tmpDir)

-- | Install a kit pack.
--
-- If there's a problem unpacking the kit pack; if the kit pack does
-- not appear to be valid; or if the user directory specified in the
-- config doesn't exist; then this action throws an exception and
-- aborts the installation -- no kits from the pack will be installed.
installKitPack :: (MonadThrow m, MonadMask m, MonadIO m, MonadReader r m, HasInstallConfig r) => ArchiveFilePath -> m ()
installKitPack archive@(ArchiveFilePath fn) =
  -- We should create the top-level kit path directory if it doesn't
  -- exist (and it doesn't by default), but we should not create the
  -- user directory if that doesn't exist, as that probably means it's
  -- wrong, or that the game isn't installed.
  do checkUserDir
     withSystemTempDir (basename fn) $ \tmpDir ->
       do unpackedKitDir <- unpackKitPack archive (UnpackDirPath tmpDir)
          udir <- view userDir
          ensureDir $ kitPath udir
          install unpackedKitDir (kitSubDir </> dirname (unpackDirPath unpackedKitDir))

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
