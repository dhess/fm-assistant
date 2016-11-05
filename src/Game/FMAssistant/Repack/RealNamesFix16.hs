{-|
Module      : Game.FMAssistant.Repack.RealNamesFix16
Description : Repack the Real Names Fix mod (for FM16)
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

module Game.FMAssistant.Repack.RealNamesFix16
       ( repackRealNamesFix16
         -- * Exceptions
       , RealNamesFix16RepackException(..)
       ) where

import Control.Conditional (unlessM)
import Control.Exception (Exception(..))
import Control.Monad (forM_)
import Control.Monad.Catch (MonadMask, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Data
import Path
       ((</>), Path, Abs, Rel, Dir, File, mkRelDir, mkRelFile, parent)
import Path.IO
       (copyDirRecur, copyFile, doesDirExist, ensureDir,
        withSystemTempDir)

import Game.FMAssistant.Mod
       (PackFilePath,
        PackAction(CreateAppDir, RemoveAppDir, CreateUserDir), packDir,
        packMod)
import Game.FMAssistant.Repack.Internal (ArchiveFilePath(..), generateModId)
import Game.FMAssistant.Repack.Unpack (unpack)
import Game.FMAssistant.Types
       (fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename, touchFile)

rnfDir :: Path Rel Dir
rnfDir = $(mkRelDir "FM16 Real Names Fix Files from sortitoutsi.net v3.2")

dbSubDir :: Path Rel Dir -> Path Rel Dir
dbSubDir dbVersionDir = $(mkRelDir "data/db") </> dbVersionDir

dbcSubDir :: Path Rel Dir -> Path Rel Dir
dbcSubDir dbVersionDir = dbSubDir dbVersionDir </> $(mkRelDir "dbc")

edtSubDir :: Path Rel Dir -> Path Rel Dir
edtSubDir dbVersionDir = dbSubDir dbVersionDir </> $(mkRelDir "edt")

lncSubDir :: Path Rel Dir -> Path Rel Dir
lncSubDir dbVersionDir = dbSubDir dbVersionDir </> $(mkRelDir "lnc")

editorDataSubDir :: Path Rel Dir
editorDataSubDir = $(mkRelDir "editor data")

rnfSubDir :: Path Rel Dir
rnfSubDir = editorDataSubDir </> rnfDir

dbVersions :: [Path Rel Dir]
dbVersions =
  [$(mkRelDir "1600")
  ,$(mkRelDir "1601")
  ,$(mkRelDir "1630")
  ]

addLncFiles :: [Path Rel File]
addLncFiles =
  [$(mkRelFile "Awards.lnc")
  ,$(mkRelFile "City, Club and Stadium Names.lnc")
  ,$(mkRelFile "Competitions and other Fixes.lnc")
  ]

removeDbcFiles :: [Path Rel File]
removeDbcFiles =
  [$(mkRelFile "Licensing.dbc")
  ,$(mkRelFile "permanent/Licensing2.dbc")
  ,$(mkRelFile "permanent/brazil_kits.dbc")
  ]

removeEdtFiles :: [Path Rel File]
removeEdtFiles =
  [$(mkRelFile "permanent/SI_team_holland.ddt")
  ,$(mkRelFile "permanent/fake.edt")
  ]

removeLncFiles :: [Path Rel File]
removeLncFiles =
  [$(mkRelFile "all/fake.lnc")
  ,$(mkRelFile "all/greek name change.lnc")
  ,$(mkRelFile "all/korean name change.lnc")
  ,$(mkRelFile "all/lic_dan_swe_fra.lnc")
  ,$(mkRelFile "all/nleague.lnc")
  ,$(mkRelFile "greek/person_name_changes.lnc")
  ]

-- | Repack the Real Names Fix pack.
--
-- Note: this mod advises the user to overwrite 3 game directories
-- with versions supplied in the RAR file, but in the end, all that
-- happens is that a few files are deleted, and a few are added. Here
-- we do the minimum required changes. (By doing so, we can also make
-- this mod work for all database versions.)
repackRealNamesFix16
  :: (MonadThrow m, MonadMask m, MonadIO m)
  => ArchiveFilePath
  -- ^ The archive file
  -> Path Abs Dir
  -- ^ The destination directory
  -> m PackFilePath
  -- ^ The output mod pack
repackRealNamesFix16 archive@(ArchiveFilePath fn) destDir =
  withSystemTempDir (basename fn) $ \unpackDir ->
    do unpack archive unpackDir
       let unpackedLncDir = unpackDir </> $(mkRelDir "lnc")
       unlessM (doesDirExist unpackedLncDir) $
         throwM $ MissingLncDir archive
       let unpackedRnfDir = unpackDir </> rnfDir
       unlessM (doesDirExist unpackedRnfDir) $
         throwM $ MissingRealNamesFix16Dir archive
       withSystemTempDir "repackRealNamesFix16" $ \tarDir ->
         let modRnfDir = tarDir </> packDir CreateUserDir </> rnfSubDir
         in do ensureDir (parent modRnfDir)
               copyDirRecur unpackedRnfDir modRnfDir
               forM_ dbVersions $ \dbVersion ->
                 let modLncDir = tarDir </> packDir CreateAppDir </> lncSubDir dbVersion
                     modRemoveDbcDir = tarDir </> packDir RemoveAppDir </> dbcSubDir dbVersion
                     modRemoveEdtDir = tarDir </> packDir RemoveAppDir </> edtSubDir dbVersion
                     modRemoveLncDir = tarDir </> packDir RemoveAppDir </> lncSubDir dbVersion
                 in  do ensureDir (parent modLncDir)
                        ensureDir (parent modRemoveDbcDir)
                        ensureDir (parent modRemoveEdtDir)
                        ensureDir (parent modRemoveLncDir)
                        forM_ addLncFiles
                              (\relFn ->
                                do let modFn = modLncDir </> relFn
                                   ensureDir (parent modFn)
                                   copyFile (unpackedLncDir </> relFn) modFn)
                        forM_ removeDbcFiles (removeModFile modRemoveDbcDir)
                        forM_ removeEdtFiles (removeModFile modRemoveEdtDir)
                        forM_ removeLncFiles (removeModFile modRemoveLncDir)
               modId <- generateModId archive
               packMod tarDir destDir modId
  where
    removeModFile :: (MonadIO m) => Path Abs Dir -> Path Rel File -> m ()
    removeModFile modDir relFile =
      do let modFn = modDir </> relFile
         ensureDir (parent modFn)
         touchFile modFn

data RealNamesFix16RepackException
  = MissingDbcDir ArchiveFilePath
    -- ^ The archive is missing the "dbc" directory
  | MissingEdtDir ArchiveFilePath
    -- ^ The archive is missing the "edt" directory
  | MissingLncDir ArchiveFilePath
    -- ^ The archive is missing the "lnc" directory
  | MissingRealNamesFix16Dir ArchiveFilePath
    -- ^ The archive is missing the Real Names Fix directory
  deriving (Eq,Typeable)

instance Show RealNamesFix16RepackException where
  show (MissingDbcDir fp) = show fp ++ ": Malformed Real Names fix mod (no \"dbc\" directory)"
  show (MissingEdtDir fp) = show fp ++ ": Malformed Real Names Fix mod (no \"edt\" directory)"
  show (MissingLncDir fp) = show fp ++ ": Malformed Real Names Fix mod (no \"lnc\" directory for Retina files)"
  show (MissingRealNamesFix16Dir fp) = show fp ++ ": Malformed Real Names Fix mod (no \"Real Names Fix\" directory for Retina files)"

instance Exception RealNamesFix16RepackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
