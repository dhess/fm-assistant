{-|
Module      : Game.FMAssistant.Repack.Skins
Description : Repack skins
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.Skins
       ( repackSkin
         -- * Exceptions
       , SkinException(..)
       ) where

import Control.Exception (Exception(..))
import Control.Monad (forM_)
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path
       ((</>), Path, Abs, Rel, Dir, File, dirname, filename, mkRelDir,
        mkRelFile)
import Path.IO
       (ensureDir, renameDir, renameFile, walkDirAccum, withSystemTempDir)

import Game.FMAssistant.Mod
       (PackAction(CreateFilesInUserDir), packDir, pack)
import Game.FMAssistant.Repack.Internal
       (ArchiveFilePath(..), Repack, generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

-- Skins are identified by a @skin_config.xml@ file and two special
-- subdirectories: @panels/@ and @fonts/@.

skinConfigFile :: Path Rel File
skinConfigFile = $(mkRelFile "skin_config.xml")

panelsSubDir :: Path Rel Dir
panelsSubDir = $(mkRelDir "panels")

fontsSubDir :: Path Rel Dir
fontsSubDir = $(mkRelDir "fonts")

-- Where the skins are stored in the modpack file.
skinSubDir :: Path Rel Dir
skinSubDir = $(mkRelDir "skins")

repackSkin :: (MonadMask m, MonadIO m) => Repack m
repackSkin archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \tmpDir -> do
    unpackedSkinDirs <- unpackSkins archive tmpDir
    withSystemTempDir "repackSkin" $ \tarDir ->
      let modDir = tarDir </> packDir CreateFilesInUserDir </> skinSubDir
      in do
        ensureDir modDir
        forM_ unpackedSkinDirs $ \skinDir ->
          let targetDir = modDir </> dirname skinDir
          in do
            ensureDir targetDir
            -- Only store what we need, ignore detritus.
            renameDir (skinDir </> fontsSubDir) (targetDir </> fontsSubDir)
            renameDir (skinDir </> panelsSubDir) (targetDir </> panelsSubDir)
            renameFile (skinDir </> skinConfigFile) (targetDir </> skinConfigFile)
        modId <- generateModId archive
        pack tarDir destDir modId

-- | Unpack an archive file assumed to contain a skin(s) to the given
-- parent directory.
--
-- If there is some problem during the unpacking of the archive, or if
-- the skin does not appear to be valid, this action will throw an
-- exception. See the 'SkinException' and 'RepackException' types
-- (although other exceptions are possible, of course, as this action
-- runs in 'MonadIO'.).
unpackSkins :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m [Path Abs Dir]
unpackSkins archive unpackDir = do
  unpack archive unpackDir
  skins <- walkDirAccum Nothing collectSkins unpackDir
  if null skins
    then throwM $ InvalidSkin archive
    else return skins
  where
    collectSkins
      :: (MonadThrow m, MonadIO m)
      => Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m [Path Abs Dir]
    collectSkins cwd subdirs files =
      -- Some skins contain detritus.
      if skinConfigFile `elem` map filename files
         && fontsSubDir `elem` map dirname subdirs
         && panelsSubDir `elem` map dirname subdirs
         then return [cwd]
         else return mempty

-- | Exceptions specific to skin packs.

data SkinException
  = InvalidSkin ArchiveFilePath
    -- ^ No valid skin subdirectories were found in the archive.
  deriving (Eq,Typeable)

instance Show SkinException where
  show (InvalidSkin fp) = show fp ++ ": invalid skin pack"

instance Exception SkinException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
