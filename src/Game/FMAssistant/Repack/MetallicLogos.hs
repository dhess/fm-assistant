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

import Control.Conditional (unlessM, whenM)
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data
import Data.Foldable (foldrM)
import Path ((</>), Path, Abs, Rel, Dir, mkRelDir, parent)
import Path.IO
       (copyDirRecur, doesDirExist, ensureDir, renameDir, withSystemTempDir)

import Game.FMAssistant.Mod
       (PackFilePath, PackAction(CreateUserDir), packDir, packMod)
import Game.FMAssistant.Repack.Internal (ArchiveFilePath(..), generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (Version(..), versionDir, versions,
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

picturesSubDir :: Path Rel Dir
picturesSubDir = $(mkRelDir "graphics/pictures")

editorDataSubDir :: Path Rel Dir
editorDataSubDir = $(mkRelDir "editor data")

identifyVersion :: (MonadIO m) => Path Abs Dir -> m (Maybe Version)
identifyVersion rootDir =
  liftIO $ foldrM subDirExists Nothing versions
  where
    subDirExists :: Version -> Maybe Version -> IO (Maybe Version)
    subDirExists version Nothing =
      do exists <- doesDirExist $ rootDir </> versionDir version
         if exists
           then return $ Just version
           else return Nothing
    subDirExists _ version = return version

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
       identifyVersion unpackDir >>= \case
         Nothing -> throwM $ UnknownVersion archive
         Just version ->
           do let unpackedPicturesDir = unpackDir </> versionDir version </> picturesSubDir
              unlessM (doesDirExist unpackedPicturesDir) $
                throwM $ MissingPicturesDir archive
              let unpackedEditorDataDir = unpackDir </> versionDir version </> editorDataSubDir
              unlessM (doesDirExist unpackedEditorDataDir) $
                throwM $ MissingEditorDataDir archive
              let unpackedRetinaDir = unpackDir </> $(mkRelDir "Optional Retina Files") </> versionDir version </> picturesSubDir
              withSystemTempDir "repackMetallicLogos" $ \tarDir ->
                let modPicturesDir = tarDir </> packDir CreateUserDir </> picturesSubDir
                    modEditorDataSubDir = tarDir </> packDir CreateUserDir </> editorDataSubDir
                in do ensureDir (parent modPicturesDir)
                      ensureDir (parent modEditorDataSubDir)
                      renameDir unpackedPicturesDir modPicturesDir
                      renameDir unpackedEditorDataDir modEditorDataSubDir
                      whenM (doesDirExist unpackedRetinaDir) $
                        copyDirRecur unpackedRetinaDir modPicturesDir
                      modId <- generateModId archive
                      packMod tarDir destDir modId

data MetallicLogosRepackException
  = UnknownVersion ArchiveFilePath
    -- ^ The logo pack is for an unknown/unsupported version of FM
  | MissingPicturesDir ArchiveFilePath
    -- ^ The archive is missing a "pictures" directory
  | MissingEditorDataDir ArchiveFilePath
    -- ^ The archive is missing an "editor data" directory
  deriving (Eq,Typeable)

instance Show MetallicLogosRepackException where
  show (UnknownVersion fp) = show fp ++ ": Logo pack is for an unsupported or unidentified version of Football Manager"
  show (MissingPicturesDir fp) = show fp ++ ": Malformed logo pack (no \"pictures\" directory)"
  show (MissingEditorDataDir fp) = show fp ++ ": Malformed logo pack (no \"editor data\" directory)"

instance Exception MetallicLogosRepackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
