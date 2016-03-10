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
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Mod.Kits
       ( installKitPack
       , validateKitPack
         -- * Kit pack-related exceptions
       , KitPackException(..)
       ) where

import Prelude hiding (FilePath)
import Control.Exception (Exception(..), throw)
import qualified Control.Foldl as Fold (length, head)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (Managed, runManaged)
import Data.Data
import Filesystem.Path.CurrentOS ((</>), FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString, filename)
import Turtle.Prelude (ls, mktree, mv, rmtree, testdir)
import Turtle.Shell (fold)

import Game.FMAssistant.Types
       (ArchiveFilePath(..), UserDirFilePath(..),
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Unpack (unpack)

-- | Kits live in a pre-determined subdirectory of the game's user
-- directory. This function constructs the path to that subdirectory,
-- given a particular 'UserDirFilePath'.
--
-- >>> :set -XOverloadedStrings
-- >>> kitPath $ UserDirFilePath "/home/dhess/Football Manager 2016"
-- FilePath "/home/dhess/Football Manager 2016/graphics/kits"
kitPath :: UserDirFilePath -> FilePath
kitPath ufp = _userDirFilePath ufp </> "graphics" </> "kits"

-- | Verify that a kit pack archive file:
--
-- * Can be successfully unpacked.
--
-- * Is properly formatted, i.e., not malformed.
--
-- A kit pack is considered to be malformed if the top-level directory
-- of the unpacked kit pack contains anything other than a single
-- subdirectory. Any other arrangement of files and/or directories in
-- the unpacked archive is considered to be bad kit pack hygiene, as
-- if you were to install more than one kit pack that is organized
-- this way, there would be no easy way to prevent one pack from
-- overwriting files in another; nor any reliable way to replace an
-- earlier version of a given pack with a later one.
--
-- If the kit pack is successfully validated, this action simply
-- returns a void result; the kit pack is not actually installed, nor
-- its unpacked contents otherwise kept around.
--
-- If the pack is found to be invalid, this action throws an
-- exception. Exceptions you should expect to occur are either of type
-- 'KitPackException' or 'Game.FMAssistant.Unpack.UnpackException'; if
-- you don't care about the particular exception that has occurred but
-- only want to 'show' it, you can catch
-- 'Game.FMAssistant.Types.SomeFMAssistantException', as this type
-- will encapsulate the range of issues that may occur with the kit
-- pack archive file. (As this action runs in 'IO', other exceptions
-- may occur, of course, but they will most likely not be an
-- indication that anything is wrong with the kit pack.)
--
-- It's not necessary to run this action prior to attempting to
-- install the kit pack, as 'installKitPack' will perform the same
-- checks as this action does. This action is provided primarily to
-- help users weed faulty kit packs out of their collections, e.g., by
-- using it as a filter on a directory full of kit pack archives.
validateKitPack :: (MonadIO m) => ArchiveFilePath -> m ()
validateKitPack archive = liftIO $ runManaged $ void $ unpackKitPack archive

-- | Install a kit pack to the given user directory. Note that this
-- action will overwrite an existing kit path of the same name (but
-- not necessarily of a different version: this depends on how the kit
-- pack was packaged).
--
-- If the kit pack is malformed, this action throws an exception and
-- aborts the installation -- no kits from the pack will be installed.
installKitPack :: (MonadIO m) => UserDirFilePath -> ArchiveFilePath -> m ()
installKitPack userDir archive = liftIO $
  -- We should create the top-level kit path directory if it doesn't
  -- exist (and it doesn't by default), but we should not create the
  -- user directory if that doesn't exist, as that probably means it's
  -- wrong, or that the game isn't installed.
  do userDirExists <- testdir $ _userDirFilePath userDir
     unless userDirExists $
       throw $ NoSuchUserDirectory userDir
     runManaged $
       do unpackedLocation <- unpackKitPack archive
          -- Remove the existing installed kit pack with the same
          -- name, if it exists.
          --
          -- Note that we use 'Filesystem.filename' to extract the
          -- directory name under which the kit pack files live
          -- (i.e., the parent directory). Technically this is a
          -- directory name, not a filename, as 'findKitPack' only
          -- returns a path to a directory; but the @system-filepath@
          -- package makes no semantic distinction between filenames
          -- and directory names at the end of paths.
          let parentDirName = Filesystem.filename unpackedLocation
          let installedLocation = kitPath userDir </> parentDirName
          exists <- testdir installedLocation
          if exists
             then rmtree installedLocation
             else mktree $ kitPath userDir
          mv unpackedLocation installedLocation

-- | Unpack an archive file assumed to contain a kit pack to a
-- temporary directory. Once unpacked, run a simple validation check
-- and, if it passes, return a ('Managed') path to the top-level kit
-- pack directory.
--
-- If the validation check fails, or if there is some problem during
-- the unpacking of the archive, this action will throw an exception.
-- See the 'KitPackException' type for exceptions specific to kit
-- packs (although other exceptions are possible, of course, as this
-- action runs in 'MonadIO'.).
unpackKitPack :: ArchiveFilePath -> Managed FilePath
unpackKitPack archive =
  do unpackedArchive <- unpack archive
     fixUp unpackedArchive
     -- Currently the validation check is quite simple: the archive must
     -- contain just a single directory and no top-level files.
     fold (ls unpackedArchive) ((,) <$> Fold.length <*> Fold.head) >>= \case
       (0, _) -> throw $ EmptyArchive archive
       (1, Just fp) ->
         do isDir <- testdir fp
            if isDir
               then return fp
               else throw $ SingleFileArchive archive
       _ -> throw $ MultipleFilesOrDirectories archive

-- | Attempt to fix up common issues with kit packs.
fixUp :: (MonadIO m) => FilePath -> m ()
fixUp fp = void $ osxFixUp fp

-- | Remove any top-level "__MACOSX" subdirectory.
osxFixUp :: (MonadIO m) => FilePath -> m FilePath
osxFixUp fp =
  let osxdir :: FilePath
      osxdir = fp </> (Filesystem.decodeString "__MACOSX")
  in
    do exists <- testdir osxdir
       when exists $
         rmtree osxdir
       return fp

data KitPackException
  = NoSuchUserDirectory UserDirFilePath
    -- ^ The specified user directory doesn't exist
  | EmptyArchive ArchiveFilePath
    -- ^ The archive is empty
  | SingleFileArchive ArchiveFilePath
    -- ^ The archive contains just a single file
  | MultipleFilesOrDirectories ArchiveFilePath
    -- ^ The archive contains multiple files or directories at the top
    -- level
  deriving (Eq,Typeable)

instance Show KitPackException where
  show (NoSuchUserDirectory fp) = show fp ++ ": The game user directory doesn't exist"
  show (EmptyArchive fp) = show fp ++ ": Malformed (kit pack is empty)"
  show (SingleFileArchive fp) = show fp ++ ": Malformed (kit pack contains just a single file)"
  show (MultipleFilesOrDirectories fp) = show fp ++ ": Malformed (multiple files/directories at top level)"

instance Exception KitPackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
