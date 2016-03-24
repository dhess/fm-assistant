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
       ( facePath
       , iconFacePath
       , installCutoutMegapack
       , installCutoutIcons
         -- * Facepack-related exceptions
       , FacePackException(..)
       , fpeGetFilePath
       ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadMask, MonadThrow, catch, throwM)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir, toFilePath)
import Path.IO (doesDirExist, ensureDir, withSystemTempDir)

import Game.FMAssistant.Install (MonadInstall(..))
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
-- >>> facePath $ UserDirPath "/home/dhess/Football Manager 2016"
-- "/home/dhess/Football Manager 2016/graphics/faces"
facePath :: UserDirPath -> Path Abs Dir
facePath ufp = _userDirPath ufp </> $(mkRelDir "graphics/faces")

-- | Icon face packs live in a pre-determined subdirectory of the
-- game's user directory. This function constructs the path to that
-- subdirectory, given a particular 'UserDirPath'.
--
-- >>> :set -XOverloadedStrings
-- >>> iconFacePath $ UserDirPath "/home/dhess/Football Manager 2016"
-- "/home/dhess/Football Manager 2016/graphics/iconfaces"
iconFacePath :: UserDirPath -> Path Abs Dir
iconFacePath ufp = _userDirPath ufp </> $(mkRelDir "graphics/iconfaces")

-- | Install the Sortitoutsi "Cutout Megapack" face pack to the given
-- user directory. Note that this action will overwrite any face pack
-- which has already been installed.
--
-- If there's a problem unpacking the face pack; if the kit pack does
-- not appear to be valid; or if the user directory doesn't exist;
-- then this action throws an exception and aborts the installation --
-- the face pack will not be installed.
installCutoutMegapack :: (MonadThrow m, MonadMask m, MonadIO m, MonadInstall m) => UserDirPath -> ArchiveFilePath -> m ()
installCutoutMegapack userDir archive = installFaces userDir archive $(mkRelDir "sortitoutsi/faces") facePath

-- | Install the Sortitoutsi "Cutout Megapack" face pack to the given
-- user directory. Note that this action will overwrite any face pack
-- which has already been installed.
--
-- If there's a problem unpacking the face pack; if the kit pack does
-- not appear to be valid; or if the user directory doesn't exist;
-- then this action throws an exception and aborts the installation --
-- the face pack will not be installed.
installCutoutIcons :: (MonadThrow m, MonadMask m, MonadIO m, MonadInstall m) => UserDirPath -> ArchiveFilePath -> m ()
installCutoutIcons userDir archive = installFaces userDir archive $(mkRelDir "sortitoutsi/iconfaces") iconFacePath

installFaces :: (MonadThrow m, MonadMask m, MonadIO m, MonadInstall m) => UserDirPath -> ArchiveFilePath -> Path Rel Dir -> (UserDirPath -> Path Abs Dir) -> m ()
installFaces userDir archive@(ArchiveFilePath fn) facesSubdir installPath =
  -- We should create the face pack directory if it doesn't exist (and
  -- it doesn't by default), but we should not create the user
  -- directory if that doesn't exist, as that probably means it's
  -- wrong, or that the game isn't installed.
  do userDirExists <- doesDirExist (_userDirPath userDir)
     unless userDirExists $
       throwM $ NoSuchUserDirectory userDir
     withSystemTempDir (basename fn) $ \tmpDir ->
       do catch (unpack archive (UnpackDirPath tmpDir))
                (\(e :: UnpackException) -> throwM $ UnpackingError archive e)
          let facesDir = tmpDir </> facesSubdir
          facesExists <- doesDirExist facesDir
          unless facesExists $
            throwM $ MissingFacesDir archive
          let installDir = installPath userDir
          ensureDir installDir
          install (UnpackDirPath facesDir) installDir

data FacePackException
  = NoSuchUserDirectory UserDirPath
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
fpeGetFilePath :: FacePackException -> FilePath
fpeGetFilePath (NoSuchUserDirectory (UserDirPath fp)) = toFilePath fp
fpeGetFilePath (UnpackingError (ArchiveFilePath fp) _) = toFilePath fp
fpeGetFilePath (EmptyArchive (ArchiveFilePath fp)) = toFilePath fp
fpeGetFilePath (MissingFacesDir (ArchiveFilePath fp)) = toFilePath fp
