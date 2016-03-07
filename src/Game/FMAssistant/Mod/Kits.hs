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
       ( -- * Kit pack types
         KitPath
         -- * Kit pack functions and combinators
       , kitPath
       , filePath
       , installKitPack
       , validateKitPack
         -- * Kit pack-related exceptions
       , KitPackException(..)
       ) where

import Prelude hiding (FilePath)
import Control.Exception (Exception(..), throw, try)
import qualified Control.Foldl as Fold (length, head)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (Managed, runManaged)
import Data.Data
import Filesystem.Path.CurrentOS ((</>), FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (filename)
import Turtle.Prelude (ls, mv, rmtree, testdir, testpath)
import Turtle.Shell (fold)

import Game.FMAssistant.Types
       (ArchiveFilePath(..), UserDirFilePath(..),
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Unpack (unpack)

-- | Paths to kits.
--
-- Note that, for type safety, you cannot construct a 'KitPath'
-- directly and must use the 'kitPath' constructor. As kits are always
-- stored in a particular subdirectory of a 'UserDirFilePath', this is
-- not a problem.
newtype KitPath =
  KitPath {_kitPath :: FilePath}
  deriving (Show,Eq,Ord,Data,Typeable)

-- | Kits live in a pre-determined subdirectory of the game's user
-- directory. This function constructs the path to that subdirectory,
-- given a particular 'UserDirFilePath'.
--
-- >>> :set -XOverloadedStrings
-- >>> kitPath $ UserDirFilePath "/home/dhess/Football Manager 2016"
-- KitPath {_kitPath = FilePath "/home/dhess/Football Manager 2016/graphics/kits"}
kitPath :: UserDirFilePath -> KitPath
kitPath ufp =
  KitPath $ _userDirFilePath ufp </> "graphics" </> "kits"

-- | Retrieve the 'FilePath' from a 'KitPath'.
-- >>> :set -XOverloadedStrings
-- >>> filePath $ kitPath $ UserDirFilePath "/home/dhess/Football Manager 2016"
-- FilePath "/home/dhess/Football Manager 2016/graphics/kits"
filePath :: KitPath -> FilePath
filePath = _kitPath

-- | Verify that a kit pack:
--
-- * Can be successfully unpacked.
--
-- * Is properly formatted, i.e., not malformed.
--
-- If the kit pack is successfully validated, this action returns
-- 'Right' @(@). Otherwise, it returns an exception value in a 'Left'.
--
-- A kit pack is considered to be malformed if the top-level directory
-- of the unpacked kit pack contains anything other than a single
-- subdirectory. Any other arrangement of files and/or directories in
-- the unpacked archive is considered to be bad kit pack hygiene, as
-- if you were to install more than one kit pack organized this way,
-- there would be no easy way to prevent one pack from overwriting
-- files in another; nor any reliable way to replace an earlier
-- version of a given pack with a later one.
--
-- Note that this action does /not/ install the kit pack, though it
-- does unpack it to a temporary directory in order to test it. (The
-- temporary directory is deleted when the action returns, or if an
-- exception is thrown during the validation process.)
--
-- It's not necessary to run this action prior to attempting to
-- install the kit pack, as 'installKitPack' will perform the same
-- checks as this action does. This action is provided primarily to
-- help users weed faulty kit packs out of their collections.
--
-- All known exceptions that could occur while validating the pack are
-- caught here; these exceptions are not propagated back to the user,
-- rather the action them in-band in a 'Left' value. However, as this
-- action runs in 'IO', other unexpected, unhandled exceptions are
-- always a possibility, of course.
validateKitPack :: (MonadIO m, Exception e) => ArchiveFilePath -> m (Either e ())
validateKitPack archive = liftIO $ try doit
  where
    doit :: (MonadIO m) => m ()
    doit = liftIO $
      do runManaged $ void $ unpackKitPack archive

-- | Install a kit pack to the given kit path. Note that this action
-- will overwrite an existing kit path of the same name (but not
-- necessarily of a different version: this depends on how the kit
-- pack was packaged).
--
-- If the kit pack is malformed, this action throws an exception and
-- aborts the installation -- no kits from the pack will be installed.
installKitPack :: (MonadIO m) => KitPath -> ArchiveFilePath -> m ()
installKitPack kp archive = liftIO $
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
       let installedLocation = _kitPath kp </> parentDirName
       exists <- testpath installedLocation
       when exists $
         rmtree installedLocation
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
     -- Currently the validation check is quite simple: the archive must
     -- contain just a single directory and no top-level files.
     fold (ls unpackedArchive) ((,) <$> Fold.length <*> Fold.head) >>= \case
       (0, _) -> throw $ EmptyArchiveFile archive
       (1, Just fp) ->
         do isDir <- testdir fp
            if isDir
               then return fp
               else throw $ MalformedArchive archive
       _ -> throw $ MalformedArchive archive

data KitPackException
  = EmptyArchiveFile ArchiveFilePath -- ^ The archive is empty
  | MalformedArchive ArchiveFilePath -- ^ The archive is malformed
  deriving (Show,Eq,Typeable)

instance Exception KitPackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
