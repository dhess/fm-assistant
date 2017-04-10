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
       (PackAction(CreateFilesInUserDir), packDir, pack)
import Game.FMAssistant.Repack.Internal
       (ArchiveFilePath(..), Repack, generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (fmAssistantExceptionToException,
        fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

picturesSubDir :: Path Rel Dir
picturesSubDir = $(mkRelDir "graphics/pictures")

editorDataSubDir :: Path Rel Dir
editorDataSubDir = $(mkRelDir "editor data")

findSubDir :: (MonadIO m) => Path Abs Dir -> m (Maybe (Path Rel Dir))
findSubDir rootDir =
  liftIO $ foldrM subDirExists Nothing [$(mkRelDir "Football Manager 2016")
                                       ,$(mkRelDir "Football Manager 2017")
                                       ,$(mkRelDir "sortitoutsi Metallic Logos")]
  where
    subDirExists :: Path Rel Dir -> Maybe (Path Rel Dir) -> IO (Maybe (Path Rel Dir))
    subDirExists subdir Nothing =
      do exists <- doesDirExist $ rootDir </> subdir
         if exists
           then return $ Just subdir
           else return Nothing
    subDirExists _ subdir = return subdir

-- | Repack the Metallic Logos pack.
repackMetallicLogos :: (MonadThrow m, MonadMask m, MonadIO m) => Repack m
repackMetallicLogos archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \unpackDir ->
    do unpack archive unpackDir
       findSubDir unpackDir >>= \case
         Nothing -> throwM $ UnknownVersion archive
         Just subdir ->
           do let unpackedPicturesDir = unpackDir </> subdir </> picturesSubDir
              unlessM (doesDirExist unpackedPicturesDir) $
                throwM $ MissingPicturesDir archive
              let unpackedEditorDataDir = unpackDir </> subdir </> editorDataSubDir
              unlessM (doesDirExist unpackedEditorDataDir) $
                throwM $ MissingEditorDataDir archive
              let unpackedRetinaDir = unpackDir </> $(mkRelDir "Optional Retina Files") </> subdir </> picturesSubDir
              withSystemTempDir "repackMetallicLogos" $ \tarDir ->
                let modPicturesDir = tarDir </> packDir CreateFilesInUserDir </> picturesSubDir
                    modEditorDataSubDir = tarDir </> packDir CreateFilesInUserDir </> editorDataSubDir
                in do ensureDir (parent modPicturesDir)
                      ensureDir (parent modEditorDataSubDir)
                      renameDir unpackedPicturesDir modPicturesDir
                      renameDir unpackedEditorDataDir modEditorDataSubDir
                      whenM (doesDirExist unpackedRetinaDir) $
                        copyDirRecur unpackedRetinaDir modPicturesDir
                      modId <- generateModId archive
                      pack tarDir destDir modId

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
