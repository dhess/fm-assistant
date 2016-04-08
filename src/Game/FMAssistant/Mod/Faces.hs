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
       ( -- * Face kit paths
         facesPath
       , iconFacesPath
         -- * Install actions
       , installCutoutMegapack
       , installCutoutIcons
         -- * Facepack-related exceptions
       , FacePackException(..)
       ) where

import Control.Conditional (unlessM)
import Control.Exception (Exception(..))
import Control.Lens
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..))
import Data.Data
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir, parent)
import Path.IO (doesDirExist, ensureDir, withSystemTempDir)

import Game.FMAssistant.Install (HasInstallConfig(..), install, checkUserDir)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), UserDirPath(..), UnpackDirPath(..),
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Unpack (unpack)
import Game.FMAssistant.Util (basename)

-- | Face packs live in a pre-determined subdirectory of the game's
-- user directory. This function constructs the path to that
-- subdirectory, given a particular 'UserDirPath'.
--
-- >>> :set -XOverloadedStrings
-- >>> facesPath $ UserDirPath "/home/dhess/Football Manager 2016"
-- "/home/dhess/Football Manager 2016/graphics/faces"
facesPath :: UserDirPath -> Path Abs Dir
facesPath ufp = userDirPath ufp </> facesSubDir

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
iconFacesPath ufp = userDirPath ufp </> iconFacesSubDir

iconFacesSubDir :: Path Rel Dir
iconFacesSubDir = $(mkRelDir "graphics/iconfaces")

-- | Install the Sortitoutsi "Cutout Megapack" face pack.
--
-- If there's a problem unpacking the face pack; if the kit pack does
-- not appear to be valid; or if the user directory specified in the
-- config doesn't exist; then this action throws an exception and
-- aborts the installation -- the face pack will not be installed.
installCutoutMegapack :: (MonadThrow m, MonadMask m, MonadIO m, MonadReader r m, HasInstallConfig r) => ArchiveFilePath -> m ()
installCutoutMegapack = installFaces $(mkRelDir "sortitoutsi/faces") facesSubDir

-- | Install the Sortitoutsi "Cutout Megapack" face pack to the given
-- user directory.
--
-- If there's a problem unpacking the face pack; if the kit pack does
-- not appear to be valid; or if the user directory specified in the
-- config doesn't exist; then this action throws an exception and
-- aborts the installation -- the face pack will not be installed.
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
  do checkUserDir
     withSystemTempDir (basename fn) $ \tmpDir ->
       do unpack archive (UnpackDirPath tmpDir)
          let facesDir = tmpDir </> srcSubDir
          unlessM (doesDirExist facesDir) $
            throwM $ MissingFacesDir archive
          -- Create the top-level faces installation directory
          udir <- view userDir
          ensureDir $ parent (userDirPath udir </> destSubDir)
          install (UnpackDirPath facesDir) destSubDir

data FacePackException
  = MissingFacesDir ArchiveFilePath
    -- ^ The archive is missing a "faces" directory
  deriving (Eq,Typeable)

instance Show FacePackException where
  show (MissingFacesDir fp) = show fp ++ ": Malformed face pack (no \"faces\" directory)"

instance Exception FacePackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
