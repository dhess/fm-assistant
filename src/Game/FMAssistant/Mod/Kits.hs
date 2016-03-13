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
{-# LANGUAGE Safe #-}

module Game.FMAssistant.Mod.Kits
       ( installKitPack
       , validateKitPack
         -- * Kit pack-related exceptions
       , KitPackException(..)
       , kpeGetFileName
       ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import qualified Control.Foldl as Fold (fold, length, head)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (release, runResourceT)
import Data.Data
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, renameDirectory, renameFile)
import System.FilePath ((</>), takeBaseName, takeFileName)

import Game.FMAssistant.Types
       (ArchiveFilePath(..), UserDirFilePath(..), archiveName,
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Unpack (UnpackException, unpack)
import Game.FMAssistant.Util (createTempDirectory, listDirectory)

-- | Kits live in a pre-determined subdirectory of the game's user
-- directory. This function constructs the path to that subdirectory,
-- given a particular 'UserDirFilePath'.
--
-- >>> :set -XOverloadedStrings
-- >>> kitPath $ UserDirFilePath "/home/dhess/Football Manager 2016"
-- "/home/dhess/Football Manager 2016/graphics/kits"
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
validateKitPack ar@(ArchiveFilePath fn) = liftIO $
  runResourceT $
    do (rkey, tmpDir) <- createTempDirectory (takeBaseName fn)
       void $ unpackKitPack ar tmpDir
       release rkey

-- | Install a kit pack to the given user directory. Note that this
-- action will overwrite an existing kit path of the same name (but
-- not necessarily of a different version: this depends on how the kit
-- pack was packaged).
--
-- If the kit pack is malformed, this action throws an exception and
-- aborts the installation -- no kits from the pack will be installed.
installKitPack :: (MonadThrow m, MonadIO m) => UserDirFilePath -> ArchiveFilePath -> m ()
installKitPack userDir archive@(ArchiveFilePath fn) = liftIO $
  -- We should create the top-level kit path directory if it doesn't
  -- exist (and it doesn't by default), but we should not create the
  -- user directory if that doesn't exist, as that probably means it's
  -- wrong, or that the game isn't installed.
  do userDirExists <- doesDirectoryExist (_userDirFilePath userDir)
     unless userDirExists $
       throwM $ NoSuchUserDirectory userDir
     runResourceT $
       do (rkey, tmpDir) <- createTempDirectory (takeBaseName fn)
          kitDir <- unpackKitPack archive tmpDir
          -- Remove the existing installed kit pack with the same
          -- name, if it exists.
          --
          -- Note that we use 'takeFileName' to extract the directory
          -- name under which the kit pack files live (i.e., the
          -- parent directory). Technically this is a directory name,
          -- not a filename, as 'findKitPack' only returns a path to a
          -- directory; but the @filepath@ package makes no semantic
          -- distinction between filenames and directory names at the
          -- end of paths.
          let parentDirName = takeFileName kitDir
          let installedLocation = kitPath userDir </> parentDirName
          exists <- liftIO $ doesDirectoryExist installedLocation
          if exists
             then liftIO $ removeDirectoryRecursive installedLocation
             else liftIO $ createDirectoryIfMissing True $ kitPath userDir
          liftIO $ renameDirectory kitDir installedLocation
          release rkey

-- | Unpack an archive file assumed to contain a kit pack to the given
-- parent directory. Once unpacked, run a simple validation check and,
-- if it passes, return a path to the top-level kit pack directory.
--
-- If the validation check fails, or if there is some problem during
-- the unpacking of the archive, this action will throw an exception.
-- See the 'KitPackException' type for exceptions specific to kit
-- packs (although other exceptions are possible, of course, as this
-- action runs in 'MonadIO'.).
unpackKitPack :: (MonadCatch m, MonadThrow m, MonadIO m) => ArchiveFilePath -> FilePath -> m FilePath
unpackKitPack archive unpackDir =
  do catch (unpack archive unpackDir)
           (\(e :: UnpackException) -> throwM $ UnpackingError archive e)
     -- Note: the pathname we return may be dependent on fix-ups.
     osxFixUp unpackDir >>= singleDirFixUp archive

-- | Remove any top-level "__MACOSX" subdirectory.
osxFixUp :: (MonadIO m) => FilePath -> m FilePath
osxFixUp unpackDir =
  let osxdir :: FilePath
      osxdir = unpackDir </> "__MACOSX"
  in
    do exists <- liftIO $ doesDirectoryExist osxdir
       when exists $
         liftIO $ removeDirectoryRecursive osxdir
       return unpackDir

-- | If the kit pack spews files all over the place, rather than
-- putting them in a single top-level directory, create a top-level
-- directory and move everything there.
singleDirFixUp :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> FilePath -> m FilePath
singleDirFixUp archive unpackDir =
  do ls <- listDirectory unpackDir
     case Fold.fold ((,) <$> Fold.length <*> Fold.head) ls of
       (0, _) -> throwM $ EmptyArchive archive
       (1, Just fp) ->
        do isDir <- liftIO $ doesDirectoryExist fp
           if isDir
              then return fp
              else throwM $ SingleFileArchive archive
       _ ->
         let fixupDir = unpackDir </> archiveName archive
         in
           do liftIO $
                do createDirectory fixupDir
                   forM_ ls $ \fn ->
                     let newFn = fixupDir </> takeFileName fn
                     in
                       do isDir <- doesDirectoryExist fn
                          if isDir
                             then renameDirectory fn newFn
                             else renameFile fn newFn
              return fixupDir

data KitPackException
  = NoSuchUserDirectory UserDirFilePath
    -- ^ The specified user directory doesn't exist
  | UnpackingError ArchiveFilePath UnpackException
    -- ^ An error occurred during the unpacking process
  | EmptyArchive ArchiveFilePath
    -- ^ The archive is empty
  | SingleFileArchive ArchiveFilePath
    -- ^ The archive contains just a single file
  deriving (Eq,Typeable)

instance Show KitPackException where
  show (NoSuchUserDirectory fp) = show fp ++ ": The game user directory doesn't exist"
  show (UnpackingError fp ue) = show fp ++ ": " ++ show ue
  show (EmptyArchive fp) = show fp ++ ": Malformed (kit pack is empty)"
  show (SingleFileArchive fp) = show fp ++ ": Malformed (kit pack contains just a single file)"

instance Exception KitPackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException

-- | The 'FilePath' associated with the given exception.
kpeGetFileName :: KitPackException -> FilePath
kpeGetFileName (NoSuchUserDirectory (UserDirFilePath fp)) = fp
kpeGetFileName (UnpackingError (ArchiveFilePath fp) _) = fp
kpeGetFileName (EmptyArchive (ArchiveFilePath fp)) = fp
kpeGetFileName (SingleFileArchive (ArchiveFilePath fp)) = fp
