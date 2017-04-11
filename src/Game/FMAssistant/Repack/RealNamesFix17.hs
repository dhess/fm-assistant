{-|
Module      : Game.FMAssistant.Repack.RealNamesFix17
Description : Repack the Real Names Fix mod for FM17
Copyright   : (c) 2017, Drew Hess
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

module Game.FMAssistant.Repack.RealNamesFix17
       ( repackRealNamesFix17
       , supported
       ) where

import Control.Conditional (unlessM)
import Control.Monad (forM_)
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Set (member, fromList)
import Path
       ((</>), Path, Rel, Dir, filename, mkRelDir, mkRelFile, parent)
import Path.IO
       (copyDirRecur, doesDirExist, ensureDir, withSystemTempDir)

import Game.FMAssistant.Mod (PackAction(..), packDir, pack)
import Game.FMAssistant.Repack.Internal
       (ArchiveFilePath(..), Repack, generateModId)
import Game.FMAssistant.Repack.RealNamesFix
       (RealNamesFixRepackException(..), dbcSubDir, editorDataSubDir,
        edtSubDir, lncSubDir)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Util (basename)

supported :: ArchiveFilePath -> Bool
supported (ArchiveFilePath fn) =
  member (filename fn) $
  fromList
    [$(mkRelFile "FM17 Real Names Fix Files from sortitoutsi.net v2.1.rar")]

dbVersions :: [Path Rel Dir]
dbVersions =
  [$(mkRelDir "1700")
  ,$(mkRelDir "1701")
  ,$(mkRelDir "1710")
  ,$(mkRelDir "1712")
  ,$(mkRelDir "1730")
  ]

-- | Repack the Real Names Fix pack.
repackRealNamesFix17 :: (MonadThrow m, MonadMask m, MonadIO m) => Repack m
repackRealNamesFix17 archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \unpackDir -> do
    unpack archive unpackDir
    let unpackedLncDir = unpackDir </> $(mkRelDir "lnc")
    unlessM (doesDirExist unpackedLncDir) $ throwM $ MissingLncDir archive
    let unpackedDbcDir = unpackDir </> $(mkRelDir "dbc")
    unlessM (doesDirExist unpackedDbcDir) $ throwM $ MissingDbcDir archive
    let unpackedEdtDir = unpackDir </> $(mkRelDir "edt")
    unlessM (doesDirExist unpackedEdtDir) $ throwM $ MissingEdtDir archive
    let unpackedEditorDataDir = unpackDir </> $(mkRelDir "editor data")
    unlessM (doesDirExist unpackedEditorDataDir) $ throwM $ MissingRnfEditorDataDir archive
    withSystemTempDir "repackRealNamesFix" $ \tarDir -> do
      let modEditorDataDir = tarDir </> packDir CreateFilesInUserDir </> editorDataSubDir
      ensureDir (parent modEditorDataDir)
      copyDirRecur unpackedEditorDataDir modEditorDataDir
      forM_ dbVersions $ \dbVersion ->
        let copyDirs =
              [ (unpackedLncDir, tarDir </> packDir CreateFilesInAppDir </> lncSubDir dbVersion)
              , (unpackedDbcDir, tarDir </> packDir CreateFilesInAppDir </> dbcSubDir dbVersion)
              , (unpackedEdtDir, tarDir </> packDir CreateFilesInAppDir </> edtSubDir dbVersion)
              ]
            rmDirs =
              [ tarDir </> packDir RemoveDirsFromAppDir </> lncSubDir dbVersion
              , tarDir </> packDir RemoveDirsFromAppDir </> dbcSubDir dbVersion
              , tarDir </> packDir RemoveDirsFromAppDir </> edtSubDir dbVersion
              ]
        in do
          forM_ rmDirs ensureDir
          forM_ copyDirs $ \(unpackedDir, copyDir) -> do
            ensureDir (parent copyDir)
            copyDirRecur unpackedDir copyDir
      modId <- generateModId archive
      pack tarDir destDir modId
