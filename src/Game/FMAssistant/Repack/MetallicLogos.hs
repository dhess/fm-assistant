{-|
Module      : Game.FMAssistant.Repack.MetallicLogos
Description : Repack the Metallic Logos mod
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

module Game.FMAssistant.Repack.MetallicLogos
       ( repackMetallicLogos
         -- * Exceptions
       , MetallicLogosRepackException(..)
       ) where

import Control.Conditional (unlessM)
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir, parent)
import Path.IO
       (copyDirRecur, doesDirExist, ensureDir, renameDir, withSystemTempDir)

import Game.FMAssistant.Mod
       (PackFilePath, PackAction(CreateUserDir), packDir, packMod)
import Game.FMAssistant.Repack.Internal (ArchiveFilePath(..), generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

picturesSubDir :: Path Rel Dir
picturesSubDir = $(mkRelDir "graphics/pictures")

editorDataSubDir :: Path Rel Dir
editorDataSubDir = $(mkRelDir "editor data")

-- | Repack the Metallic Logos pack.
repackMetallicLogos
  :: (MonadThrow m, MonadMask m, MonadIO m)
  => ArchiveFilePath
  -- ^ The archive file
  -> Path Abs Dir
  -- ^ The destination directory
  -> m PackFilePath
  -- ^ The output mod pack
repackMetallicLogos archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \unpackDir ->
    do unpack archive unpackDir
       let unpackedPicturesDir = unpackDir </> $(mkRelDir "Football Manager 2016/graphics/pictures")
       unlessM (doesDirExist unpackedPicturesDir) $
         throwM $ MissingPicturesDir archive
       let unpackedEditorDataDir = unpackDir </> $(mkRelDir "Football Manager 2016/editor data")
       unlessM (doesDirExist unpackedEditorDataDir) $
         throwM $ MissingEditorDataDir archive
       let unpackedRetinaDir = unpackDir </> $(mkRelDir "Optional Retina Files/Football Manager 2016/graphics/pictures")
       unlessM (doesDirExist unpackedRetinaDir) $
         throwM $ MissingRetinaDir archive
       withSystemTempDir "repackMetallicLogos" $ \tarDir ->
         let modPicturesDir = tarDir </> packDir CreateUserDir </> picturesSubDir
             modEditorDataSubDir = tarDir </> packDir CreateUserDir </> editorDataSubDir
         in do ensureDir (parent modPicturesDir)
               ensureDir (parent modEditorDataSubDir)
               renameDir unpackedPicturesDir modPicturesDir
               renameDir unpackedEditorDataDir modEditorDataSubDir
               copyDirRecur unpackedRetinaDir modPicturesDir
               modId <- generateModId archive
               packMod tarDir destDir modId

data MetallicLogosRepackException
  = MissingPicturesDir ArchiveFilePath
    -- ^ The archive is missing a "pictures" directory
  | MissingEditorDataDir ArchiveFilePath
    -- ^ The archive is missing an "editor data" directory
  | MissingRetinaDir ArchiveFilePath
    -- ^ The archive is missing an "pictures" directory for Retina files
  deriving (Eq,Typeable)

instance Show MetallicLogosRepackException where
  show (MissingPicturesDir fp) = show fp ++ ": Malformed face pack (no \"pictures\" directory)"
  show (MissingEditorDataDir fp) = show fp ++ ": Malformed face pack (no \"editor data\" directory)"
  show (MissingRetinaDir fp) = show fp ++ ": Malformed face pack (no \"pictures\" directory for Retina files)"

instance Exception MetallicLogosRepackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
