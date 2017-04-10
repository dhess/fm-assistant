{-|
Module      : Game.FMAssistant.Mod
Description : Game mods
Copyright   : (c) 2017, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Mod
       ( -- * Mod configuration
         Config(..)
       , appDir
       , userDir
       , backupDir
       , version
       , HasConfig

         -- * The mod monad
       , ModT(..)
       , Mod
       , runMod

         -- * Mod actions and types
       , PackFilePath(..)
       , packId
       , packBackupDir
       , PackAction(..)
       , packDir
       , pack
       , unpack
       , validate
       , install

         -- * Exceptions
       , ModException
       ) where

import qualified Codec.Archive.Tar as Tar (pack, read, unpack, write)
import qualified Codec.Compression.Lzma as Lzma (compress, decompress)
import Control.Applicative (Alternative)
import Control.Lens (makeClassy, view)
import Control.Exception (Exception(..))
import Control.Monad (MonadPlus, forM_, unless, void, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Data
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member)
import Path
       ((</>), Path, Abs, Rel, Dir, File, dirname, isParentOf, mkRelDir,
        parent, parseRelDir, parseRelFile, toFilePath)
import Path.IO
       (AnyPath(..), copyDirRecur, doesDirExist, ensureDir,
        ignoringAbsence, listDir, listDirRecur, renameFile,
        withSystemTempDir)

import Game.FMAssistant.Types
       (AppDirPath(..), BackupDirPath(..), UserDirPath(..), Version(..),
        fmAssistantExceptionToException, fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

-- | Mod configuration.
data Config =
  Config {_appDir :: AppDirPath
         ,_userDir :: UserDirPath
         ,_backupDir :: BackupDirPath
         ,_version :: Version}

makeClassy ''Config

-- | The mod monad transformer, for building your own applications.
newtype ModT m a =
  ModT { runModT :: m a }
  deriving (Functor, Alternative, Applicative, Monad, MonadFix, MonadPlus, MonadThrow, MonadCatch, MonadMask, MonadCont, MonadIO, MonadReader r, MonadError e, MonadWriter w, MonadState s, MonadRWS r w s)

instance MonadTrans ModT where
  lift = ModT

-- | A convenient alias for the default mod monad ('ModT' over 'IO').
type Mod a = ModT (ReaderT Config IO) a

-- | Run a mod action in 'IO'.
runMod :: Config -> Mod a -> IO a
runMod c a = runReaderT (runModT a) c

-- | The type for paths which point to a pack file.
newtype PackFilePath =
  PackFilePath {packFilePath :: Path Abs File}
  deriving (Eq,Show,Ord,Typeable)

-- | Return (as a 'String') the ID (name and version number) of an
-- pack file; that is, given a 'PackFilePath', strip off the pathname
-- portion and the trailing file suffix, and return what remains.
--
-- >>> :set -XOverloadedStrings
-- >>> import Path (parseAbsFile)
-- >>> bazPath <- parseAbsFile "/foo/bar/baz.20160401.fmax"
-- >>> packId $ PackFilePath bazPath
-- "baz.20160401"
packId :: PackFilePath -> String
packId = basename . packFilePath

-- | Uniquely identify a backup directory for the given pack file.
packBackupDir :: (MonadThrow m) => PackFilePath -> BackupDirPath -> m (Path Abs Dir)
packBackupDir packFile backupParentDir =
  do backupSubDir <- parseRelDir $ basename (packFilePath packFile)
     return $ backupDirPath backupParentDir </> backupSubDir

-- | All possible actions when installing a mod.
--
-- Note: order matters here! Actions will be processed in the order
-- specified by the type. (Conversely, when uninstalling a mod,
-- reversing the actions will be done in the reverse of the order
-- specified by the type.)
data PackAction
  = RemoveFilesFromAppDir
  | CreateFilesInAppDir
  | CreateFilesInUserDir
  deriving (Eq,Ord,Enum,Bounded,Show)

allActions :: [PackAction]
allActions = [minBound ..]

packDir :: PackAction -> Path Rel Dir
packDir RemoveFilesFromAppDir = $(mkRelDir "remove_app")
packDir CreateFilesInAppDir = $(mkRelDir "create_app")
packDir CreateFilesInUserDir = $(mkRelDir "create_user")

-- | The set of valid top-level directories in a mod pack.
validTLDs :: Set (Path Rel Dir)
validTLDs = Set.fromList $ map packDir allActions

-- | Validate a mod pack: throws an exception if any problems are
-- detected, otherwise returns the void result.
validate
  :: (MonadIO m, MonadMask m)
  => PackFilePath
     -- ^ The mod pack
  -> m ()
validate packFile =
  withSystemTempDir "validate" $ \tmpDir ->
    do unpack packFile tmpDir
       void $ validate' tmpDir

-- | (Strictly) validate an unpacked mod's contents, returning the
-- list of top-level mod subdirectories if the mod is valid, throwing
-- an exception if not.
validate'
  :: (MonadIO m, MonadThrow m)
  => Path Abs Dir
  -- ^ Location of the unpacked mod
  -> m [Path Rel Dir]
  -- ^ The list of top-level mod subdirectories
validate' modDir =
  listDir modDir >>= \case
    ([], []) -> throwM NoContents
    (absDirs, []) ->
      let dirs = map dirname absDirs
          invalid = filter (\x -> not $ Set.member x validTLDs) dirs
      in case invalid of
           (x:_) -> throwM $ InvalidTopLevelDirectory x
           _ -> return $! dirs
    (_, _) -> throwM TopLevelFiles

pack
  :: (MonadIO m, MonadThrow m)
  => Path Abs Dir
  -- ^ The top-level directory where the mod contents live
  -> Path Abs Dir
  -- ^ The destination directory where the packed mod will be created
  -> String
  -- ^ The mod name/ID
  -> m PackFilePath
  -- ^ The path to the created mod pack
pack srcDir destDir modName =
  do dirs <- validate' srcDir
     tarFileName <- parseRelFile (modName ++ ".fmax")
     let tarPath = destDir </> tarFileName
     createTar tarPath dirs
     return $ PackFilePath tarPath
  where
    -- XXX TODO: refuse to write symbolic links.
    createTar :: (MonadIO m) => Path Abs File -> [Path Rel Dir] -> m ()
    createTar tarFile dirs = liftIO $
      do entries <- Tar.pack (toFilePath srcDir) (map toFilePath dirs)
         BL.writeFile (toFilePath tarFile) $ Lzma.compress $ Tar.write entries

unpack
  :: (MonadIO m)
  => PackFilePath
     -- ^ The mod pack file
  -> Path Abs Dir
     -- ^ Where to unpack the mod pack file
  -> m ()
unpack (PackFilePath pf) unpackDir = liftIO $
  -- XXX TODO: refuse to unpack symbolic links.
  do entries <- (Tar.read . Lzma.decompress) <$> BL.readFile (toFilePath pf)
     Tar.unpack (toFilePath unpackDir) entries

install
  :: (MonadIO m, MonadMask m, MonadReader r m, HasConfig r)
  => PackFilePath
     -- ^ Location of the packed mod
  -> m ()
install packFile =
  withSystemTempDir "install" install'
  where
    install' :: (MonadIO m, MonadCatch m, MonadReader r m, HasConfig r) => Path Abs Dir -> m ()
    install' unpackDir =
      do unpack packFile unpackDir
         void $ validate' unpackDir
         -- Try each pack action, in order.
         --
         -- XXX TODO: should back out of install if an exception
         -- occurs at any step.
         forM_ allActions
               (\action ->
                 let subDir = unpackDir </> packDir action
                 in do exists <- doesDirExist subDir
                       when exists $ go action subDir)
    go :: (MonadIO m, MonadCatch m, MonadReader r m, HasConfig r) => PackAction -> Path Abs Dir -> m ()
    go RemoveFilesFromAppDir subDir =
      do adir <- view appDir
         checkAppDir adir
         -- XXX TODO: need a better method for uniquely identifying
         -- the mod; use the hash, perhaps?
         bdir <- view backupDir
         modBackupDir <- packBackupDir packFile bdir
         ensureDir modBackupDir
         (_, files) <- listDirRecur subDir
         forM_ files
               (\fn ->
                  -- For security purposes, make sure the file to
                  -- remove actually lives in the app directory!
                  do relFn <- makeRelative subDir fn
                     let targetFile = appDirPath adir </> relFn
                     unless (isParentOf (appDirPath adir) targetFile) $
                       throwM $ InvalidPath (packDir RemoveFilesFromAppDir </> relFn)
                     let backupFile = modBackupDir </> relFn
                     unless (isParentOf (backupDirPath bdir) backupFile) $
                       throwM $ InvalidPath (packDir RemoveFilesFromAppDir </> relFn)
                     ensureDir (parent backupFile)
                     ignoringAbsence $ renameFile targetFile backupFile)
    go CreateFilesInAppDir subDir =
      do adir <- view appDir
         checkAppDir adir
         copyDirRecur subDir (appDirPath adir)
    go CreateFilesInUserDir subDir =
      do udir <- view userDir
         checkUserDir udir
         copyDirRecur subDir (userDirPath udir)

-- | Check that the user directory exists, throwing
-- 'NoSuchUserDirectory' if it does not.
checkUserDir :: (MonadThrow m, MonadIO m) => UserDirPath -> m ()
checkUserDir d =
  do exists <- doesDirExist (userDirPath d)
     unless exists $
       throwM $ NoSuchUserDirectory d

-- | Check that the app directory exists, throwing
-- 'NoSuchAppDirectory' if it does not.
checkAppDir :: (MonadThrow m, MonadIO m) => AppDirPath -> m ()
checkAppDir d =
  do exists <- doesDirExist (appDirPath d)
     unless exists $
       throwM $ NoSuchAppDirectory d

-- | Exceptions which can occur while packing, installing, or
-- validating a mod.
data ModException
  = NoContents
    -- ^ The mod directory is empty
  | TopLevelFiles
    -- ^ The mod directory contains top-level files
  | InvalidTopLevelDirectory (Path Rel Dir)
    -- ^ The mod directory contains an invalid top-level directory
  | NoSuchUserDirectory UserDirPath
    -- ^ The user directory doesn't exist
  | NoSuchAppDirectory AppDirPath
    -- ^ The app directory doesn't exist
  | InvalidPath (Path Rel File)
    -- ^ The mod contains a path to an invalid location
  deriving (Eq,Typeable)

instance Show ModException where
  show NoContents = "The mod directory is empty"
  show TopLevelFiles = "The mod directory contains top-level files"
  show (InvalidTopLevelDirectory dir) = "The mod directory contains an invalid top-level directory ( " ++ show dir ++ ")"
  show (NoSuchUserDirectory fp) = show fp ++ ": The game user directory doesn't exist"
  show (NoSuchAppDirectory fp) = show fp ++ ": The game application directory doesn't exist"
  show (InvalidPath fp) = show fp ++ ": Invalid/insecure path found in mod pack"

instance Exception ModException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
