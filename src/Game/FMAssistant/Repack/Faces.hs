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
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir, parseRelDir)
import Path.IO
       (doesDirExist, ensureDir, renameDir, withSystemTempDir)

import Game.FMAssistant.Mod
       (PackFilePath, PackAction(CreateUserDir), packDir, packMod)
import Game.FMAssistant.Repack.Internal (generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (ArchiveFilePath(..), UnpackDirPath(..), archiveName,
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

facesSubDir :: Path Rel Dir
facesSubDir = $(mkRelDir "graphics/faces")

iconFacesSubDir :: Path Rel Dir
iconFacesSubDir = $(mkRelDir "graphics/iconfaces")

-- | Repack the Sortitoutsi "Cutout Megapack" face pack.
repackCutoutMegapack :: (MonadMask m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m PackFilePath
repackCutoutMegapack = repackFaces $(mkRelDir "sortitoutsi/faces") facesSubDir

-- | Repack the Sortitoutsi "Cutout Megapack" face pack.
repackCutoutIcons :: (MonadMask m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m PackFilePath
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
    do unpack archive (UnpackDirPath unpackDir)
       let facesDir = unpackDir </> modSubDir
       unlessM (doesDirExist facesDir) $
         throwM $ MissingFacesDir archive
       withSystemTempDir "repackFacePack" $ \tarDir ->
         let modParentDir = tarDir </> packDir CreateUserDir </> packSubDir
         in do ensureDir modParentDir
               modDir <- parseRelDir $ archiveName archive
               renameDir unpackDir (modParentDir </> modDir)
               modId <- generateModId archive
               packMod tarDir destDir modId

data FacePackRepackException
  = MissingFacesDir ArchiveFilePath
    -- ^ The archive is missing a "faces" directory
  deriving (Eq,Typeable)

instance Show FacePackRepackException where
  show (MissingFacesDir fp) = show fp ++ ": Malformed face pack (no \"faces\" directory)"

instance Exception FacePackRepackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
