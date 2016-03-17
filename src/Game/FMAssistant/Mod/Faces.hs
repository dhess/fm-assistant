{-|
Module      : Game.FMAssistant.Mod.Faces
Description : Types and functions for dealing with facepacks
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

module Game.FMAssistant.Mod.Faces
       ( facePath
       , iconFacePath
       , installCutoutMegapack
       , installCutoutIcons
         -- * Facepack-related exceptions
       , FacePackException(..)
       , fpeGetFileName
       ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow, catch, throwM)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (release, runResourceT)
import Data.Data
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, renameDirectory)
import System.FilePath ((</>), takeBaseName, takeDirectory)

import Game.FMAssistant.Types
       (ArchiveFilePath(..), UserDirFilePath(..),
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Unpack (UnpackException, unpack)
import Game.FMAssistant.Util (createTempDirectory)

-- | Face packs live in a pre-determined subdirectory of the game's
-- user directory. This function constructs the path to that
-- subdirectory, given a particular 'UserDirFilePath'.
--
-- >>> :set -XOverloadedStrings
-- >>> facePath $ UserDirFilePath "/home/dhess/Football Manager 2016"
-- "/home/dhess/Football Manager 2016/graphics/faces"
facePath :: UserDirFilePath -> FilePath
facePath ufp = _userDirFilePath ufp </> "graphics" </> "faces"

-- | Icon face packs live in a pre-determined subdirectory of the
-- game's user directory. This function constructs the path to that
-- subdirectory, given a particular 'UserDirFilePath'.
--
-- >>> :set -XOverloadedStrings
-- >>> iconFacePath $ UserDirFilePath "/home/dhess/Football Manager 2016"
-- "/home/dhess/Football Manager 2016/graphics/iconfaces"
iconFacePath :: UserDirFilePath -> FilePath
iconFacePath ufp = _userDirFilePath ufp </> "graphics" </> "iconfaces"

-- | Install the Sortitoutsi "Cutout Megapack" face pack to the given
-- user directory. Note that this action will overwrite any face pack
-- which has already been installed.
--
-- If there's a problem unpacking the face pack; if the kit pack does
-- not appear to be valid; or if the user directory doesn't exist;
-- then this action throws an exception and aborts the installation --
-- the face pack will not be installed.
installCutoutMegapack :: (MonadThrow m, MonadIO m) => UserDirFilePath -> ArchiveFilePath -> m ()
installCutoutMegapack userDir archive = installFaces userDir archive ("sortitoutsi" </> "faces") facePath

-- | Install the Sortitoutsi "Cutout Megapack" face pack to the given
-- user directory. Note that this action will overwrite any face pack
-- which has already been installed.
--
-- If there's a problem unpacking the face pack; if the kit pack does
-- not appear to be valid; or if the user directory doesn't exist;
-- then this action throws an exception and aborts the installation --
-- the face pack will not be installed.
installCutoutIcons :: (MonadThrow m, MonadIO m) => UserDirFilePath -> ArchiveFilePath -> m ()
installCutoutIcons userDir archive = installFaces userDir archive ("sortitoutsi" </> "iconfaces") iconFacePath

installFaces :: (MonadThrow m, MonadIO m) => UserDirFilePath -> ArchiveFilePath -> FilePath -> (UserDirFilePath -> FilePath) -> m ()
installFaces userDir archive@(ArchiveFilePath fn) facesSubdir installPath = liftIO $
  -- We should create the face pack directory if it doesn't exist (and
  -- it doesn't by default), but we should not create the user
  -- directory if that doesn't exist, as that probably means it's
  -- wrong, or that the game isn't installed.
  do userDirExists <- doesDirectoryExist (_userDirFilePath userDir)
     unless userDirExists $
       throwM $ NoSuchUserDirectory userDir
     runResourceT $
       do (rkey, tmpDir) <- createTempDirectory (takeBaseName fn)
          catch (unpack archive tmpDir)
                (\(e :: UnpackException) -> throwM $ UnpackingError archive e)
          let facesDir = tmpDir </> facesSubdir
          facesExists <- liftIO $ doesDirectoryExist facesDir
          unless facesExists $
            throwM $ MissingFacesDir archive
          let installedLocation = installPath userDir
          targetExists <- liftIO $ doesDirectoryExist installedLocation
          if targetExists
            then liftIO $ removeDirectoryRecursive installedLocation
            else liftIO $ createDirectoryIfMissing True $ takeDirectory installedLocation
          liftIO $ renameDirectory facesDir installedLocation
          release rkey

data FacePackException
  = NoSuchUserDirectory UserDirFilePath
    -- ^ The specified user directory doesn't exist
  | UnpackingError ArchiveFilePath UnpackException
    -- ^ An error occurred during the unpacking process
  | EmptyArchive ArchiveFilePath
    -- ^ The archive is empty
  | MissingFacesDir ArchiveFilePath
    -- ^ The archive is missing a "faces" directory
  deriving (Eq,Typeable)

instance Show FacePackException where
  show (NoSuchUserDirectory fp) = show fp ++ ": The game user directory doesn't exist"
  show (UnpackingError fp ue) = show fp ++ ": " ++ show ue
  show (EmptyArchive fp) = show fp ++ ": Malformed (face pack is empty)"
  show (MissingFacesDir fp) = show fp ++ ": Malformed (face pack doesn't have a \"faces\" directory)"

instance Exception FacePackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException

-- | The 'FilePath' associated with the given exception.
fpeGetFileName :: FacePackException -> FilePath
fpeGetFileName (NoSuchUserDirectory (UserDirFilePath fp)) = fp
fpeGetFileName (UnpackingError (ArchiveFilePath fp) _) = fp
fpeGetFileName (EmptyArchive (ArchiveFilePath fp)) = fp
fpeGetFileName (MissingFacesDir (ArchiveFilePath fp)) = fp
