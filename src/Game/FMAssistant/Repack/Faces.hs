{-|
Module      : Game.FMAssistant.Repack.Faces
Description : Repack face packs
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.Faces
       ( repackCutoutIcons
       , repackCutoutMegapack
         -- * Exceptions
       , FacePackRepackException(..)
       ) where

import Control.Conditional (unlessM)
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir, parent)
import Path.IO
       (doesDirExist, ensureDir, renameDir, withSystemTempDir)

import Game.FMAssistant.Mod
       (PackFilePath, PackAction(CreateFilesInUserDir), packDir, pack)
import Game.FMAssistant.Repack.Internal
       (ArchiveFilePath(..), Repack, generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

facesSubDir :: Path Rel Dir
facesSubDir = $(mkRelDir "graphics/faces")

iconFacesSubDir :: Path Rel Dir
iconFacesSubDir = $(mkRelDir "graphics/iconfaces")

-- | Repack the Sortitoutsi "Cutout Megapack" face pack.
repackCutoutMegapack :: (MonadMask m, MonadIO m) => Repack m
repackCutoutMegapack = repackFaces $(mkRelDir "sortitoutsi/faces") facesSubDir

-- | Repack the Sortitoutsi "Cutout Megapack" face pack.
repackCutoutIcons :: (MonadMask m, MonadIO m) => Repack m
repackCutoutIcons = repackFaces $(mkRelDir "sortitoutsi/iconfaces") iconFacesSubDir

repackFaces
  :: (MonadThrow m, MonadMask m, MonadIO m)
  => Path Rel Dir
  -- ^ The unpacked face source subdirectory
  -> Path Rel Dir
  -- ^ The face destination subdirectory in the mod pack
  -> ArchiveFilePath
  -- ^ The archive file
  -> Path Abs Dir
  -- ^ The destination directory
  -> m PackFilePath
repackFaces modSubDir packSubDir archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \unpackDir ->
    do unpack archive unpackDir
       let facesDir = unpackDir </> modSubDir
       unlessM (doesDirExist facesDir) $
         throwM $ MissingFacesDir archive
       withSystemTempDir "repackFacePack" $ \tarDir ->
         let modDir = tarDir </> packDir CreateFilesInUserDir </> packSubDir
         in do ensureDir (parent modDir)
               renameDir unpackDir modDir
               modId <- generateModId archive
               pack tarDir destDir modId

data FacePackRepackException
  = MissingFacesDir ArchiveFilePath
    -- ^ The archive is missing a "faces" directory
  deriving (Eq,Typeable)

instance Show FacePackRepackException where
  show (MissingFacesDir fp) = show fp ++ ": Malformed face pack (no \"faces\" directory)"

instance Exception FacePackRepackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
