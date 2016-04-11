{-|
Module      : Game.FMAssistant.Repack.Trophies
Description : Repack trophy packs (graphics)
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Repack.Trophies
       ( repackTrophiesPack
       ) where

import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Path ((</>), Path, Abs, Rel, Dir, dirname, mkRelDir, parseRelDir)
import Path.IO (ensureDir, listDir, renameDir, withSystemTempDir)

import Game.FMAssistant.Mod
       (PackFilePath, PackAction(CreateUserDir), packDir, packMod)
import Game.FMAssistant.Repack.Internal
       (ArchiveFilePath(..), RepackException(..), archiveName,
        generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Util (basename)

trophiesSubDir :: Path Rel Dir
trophiesSubDir = $(mkRelDir "graphics/pictures/trophies")

repackTrophiesPack :: (MonadMask m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m PackFilePath
repackTrophiesPack archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \tmpDir ->
    do unpackedTrophiesDir <- unpackTrophiesPack archive tmpDir
       withSystemTempDir "repackTrophiesPack" $ \tarDir ->
         let modParentDir = tarDir </> packDir CreateUserDir </> trophiesSubDir
         in do ensureDir modParentDir
               modDir <- parseRelDir $ archiveName archive
               renameDir unpackedTrophiesDir (modParentDir </> modDir)
               modId <- generateModId archive
               packMod tarDir destDir modId

magicDirName :: Path Rel Dir
magicDirName = $(mkRelDir "trophies")

unpackTrophiesPack :: (MonadThrow m, MonadIO m) => ArchiveFilePath -> Path Abs Dir -> m (Path Abs Dir)
unpackTrophiesPack archive unpackDir =
  do unpack archive unpackDir
     listDir unpackDir >>= \case
       ([], []) -> throwM $ EmptyArchive archive
       ([], [_]) -> throwM $ SingleFileArchive archive
       ([dir], []) ->
         if dirname dir == magicDirName
            then return dir
            else throwM $ UnexpectedContents archive
       _ -> throwM $ UnexpectedContents archive
