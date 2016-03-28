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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Mod.Faces
       ( facesPath
       , iconFacesPath
       , installCutoutMegapack
       , installCutoutIcons
         -- * Facepack-related exceptions
       , FacePackException(..)
       , fpeGetFilePath
       ) where

import Control.Exception (Exception(..))
import Control.Lens
import Control.Monad.Catch (MonadMask, MonadThrow, catch, throwM)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..))
import Data.Data
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir, parent, toFilePath)
import Path.IO (doesDirExist, ensureDir, withSystemTempDir)

import Game.FMAssistant.Install (HasInstallConfig, install, userDir)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), UserDirPath(..), UnpackDirPath(..),
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Unpack (UnpackException, unpack)
import Game.FMAssistant.Util (basename)

-- | Face packs live in a pre-determined subdirectory of the game's
-- user directory. This function constructs the path to that
-- subdirectory, given a particular 'UserDirPath'.
--
-- >>> :set -XOverloadedStrings
-- >>> facesPath $ UserDirPath "/home/dhess/Football Manager 2016"
-- "/home/dhess/Football Manager 2016/graphics/faces"
facesPath :: UserDirPath -> Path Abs Dir
facesPath ufp = _userDirPath ufp </> facesSubDir

facesSubDir :: Path Rel Dir
facesSubDir = $(mkRelDir "graphics/faces")

-- | Icon face packs live in a pre-determined subdirectory of the
-- game's user directory. This function constructs the path to that
-- subdirectory, given a particular 'UserDirPath'.
--
-- >>> :set -XOverloadedStrings
-- >>> iconFacesPath $ UserDirPath "/home/dhess/Football Manager 2016"
-- "/home/dhess/Football Manager 2016/graphics/iconfaces"
iconFacesPath :: UserDirPath -> Path Abs Dir
iconFacesPath ufp = _userDirPath ufp </> iconFacesSubDir

iconFacesSubDir :: Path Rel Dir
iconFacesSubDir = $(mkRelDir "graphics/iconfaces")

-- | Install the Sortitoutsi "Cutout Megapack" face pack to the given
-- user directory. Note that this action will overwrite any face pack
-- which has already been installed.
--
-- If there's a problem unpacking the face pack; if the kit pack does
-- not appear to be valid; or if the user directory doesn't exist;
-- then this action throws an exception and aborts the installation --
-- the face pack will not be installed.
installCutoutMegapack :: (MonadThrow m, MonadMask m, MonadIO m, MonadReader r m, HasInstallConfig r) => ArchiveFilePath -> m ()
installCutoutMegapack = installFaces $(mkRelDir "sortitoutsi/faces") facesSubDir

-- | Install the Sortitoutsi "Cutout Megapack" face pack to the given
-- user directory. Note that this action will overwrite any face pack
-- which has already been installed.
--
-- If there's a problem unpacking the face pack; if the kit pack does
-- not appear to be valid; or if the user directory doesn't exist;
-- then this action throws an exception and aborts the installation --
-- the face pack will not be installed.
installCutoutIcons :: (MonadThrow m, MonadMask m, MonadIO m, MonadReader r m, HasInstallConfig r) => ArchiveFilePath -> m ()
installCutoutIcons = installFaces $(mkRelDir "sortitoutsi/iconfaces") iconFacesSubDir

installFaces
  :: (MonadThrow m, MonadMask m, MonadIO m, MonadReader r m, HasInstallConfig r)
  => Path Rel Dir
  -- ^ The unpacked face source subdirectory
  -> Path Rel Dir
  -- ^ The face destination subdirectory
  -> ArchiveFilePath
  -- ^ The archive file
  -> m ()
installFaces srcSubDir destSubDir archive@(ArchiveFilePath fn) =
  -- We should create the face pack directory if it doesn't exist (and
  -- it doesn't by default), but we should not create the user
  -- directory if that doesn't exist, as that probably means it's
  -- wrong, or that the game isn't installed.
  do rdr <- ask
     let udir = rdr ^. userDir
     userDirExists <- doesDirExist $ _userDirPath udir
     unless userDirExists $
       throwM $ NoSuchUserDirectory udir
     withSystemTempDir (basename fn) $ \tmpDir ->
       do catch (unpack archive (UnpackDirPath tmpDir))
                (\(e :: UnpackException) -> throwM $ UnpackingError archive e)
          let facesDir = tmpDir </> srcSubDir
          facesExists <- doesDirExist facesDir
          unless facesExists $
            throwM $ MissingFacesDir archive
          -- Create the top-level faces installation directory
          ensureDir $ parent (_userDirPath udir </> destSubDir)
          install (UnpackDirPath facesDir) destSubDir

data FacePackException
  = NoSuchUserDirectory UserDirPath
    -- ^ The specified user directory doesn't exist
  | UnpackingError ArchiveFilePath UnpackException
    -- ^ An error occurred during the unpacking process
  | MissingFacesDir ArchiveFilePath
    -- ^ The archive is missing a "faces" directory
  deriving (Eq,Typeable)

instance Show FacePackException where
  show (NoSuchUserDirectory fp) = show fp ++ ": The game user directory doesn't exist"
  show (UnpackingError fp ue) = show fp ++ ": " ++ show ue
  show (MissingFacesDir fp) = show fp ++ ": Malformed (face pack doesn't have a \"faces\" directory)"

instance Exception FacePackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException

-- | The 'FilePath' associated with the given exception.
fpeGetFilePath :: FacePackException -> FilePath
fpeGetFilePath (NoSuchUserDirectory (UserDirPath fp)) = toFilePath fp
fpeGetFilePath (UnpackingError (ArchiveFilePath fp) _) = toFilePath fp
fpeGetFilePath (MissingFacesDir (ArchiveFilePath fp)) = toFilePath fp
