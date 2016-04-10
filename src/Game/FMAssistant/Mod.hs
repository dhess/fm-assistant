{-|
Module      : Game.FMAssistant.Mod
Description : Game mods
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

module Game.FMAssistant.Mod
       ( PackFilePath(..)
       , packId
       , PackAction(..)
       , packDir
       , packMod
       , unpackMod
       , validateMod
       , installMod
         -- * Exceptions
       , ModException
       ) where

import qualified Codec.Archive.Tar as Tar (pack, read, unpack, write)
import qualified Codec.Compression.Lzma as Lzma (compress, decompress)
import Control.Exception (Exception(..))
import Control.Monad (forM_, liftM, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Data
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member)
import Path ((</>), Path, Abs, Rel, Dir, File, dirname, mkRelDir, parseRelFile, toFilePath)
import Path.IO (copyDirRecur, doesDirExist, listDir, withSystemTempDir)

import Game.FMAssistant.Types
       (UserDirPath(..), fmAssistantExceptionToException,
        fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

-- | The type for paths which point to a pack file.
newtype PackFilePath =
  PackFilePath {packFilePath :: Path Abs File}
  deriving (Eq,Show,Ord,Typeable)

-- | Return (as a 'String') the ID (name and version number) of an
-- pack file; that is, given a 'PackFilePath', strip off the pathname
-- portion and the trailing file suffix, and return what remains.
--
-- >>> :set -XOverloadedStrings
-- >>> bazPath <- parseAbsFile "/foo/bar/baz.20160401.fmax"
-- >>> packId $ PackFilePath bazPath
-- "baz.20160401"
packId :: PackFilePath -> String
packId = basename . packFilePath

-- | All possible actions when installing a mod.
--
-- Note: order matters here! Actions will be processed in the order
-- specified by the type. (Conversely, when uninstalling a mod,
-- reversing the actions will be done in the reverse of the order
-- specified by the type.)
data PackAction =
  CreateUserDir
  deriving (Eq,Ord,Enum,Bounded,Show)

allActions :: [PackAction]
allActions = [minBound ..]

packDir :: PackAction -> Path Rel Dir
packDir CreateUserDir = $(mkRelDir "create_user")

-- | The set of valid top-level directories in a mod pack.
validTLDs :: Set (Path Rel Dir)
validTLDs = Set.fromList $ map packDir allActions

-- | Validate a mod pack: throws an exception if any problems are
-- detected, otherwise returns the void result.
validateMod
  :: (MonadIO m, MonadThrow m, MonadMask m)
  => PackFilePath
  -- ^ The mod pack
  -> m ()
validateMod packFile =
  withSystemTempDir "validateMod" $ \tmpDir ->
    do unpackMod packFile tmpDir
       void $ validateMod' tmpDir

-- | (Strictly) validate an unpacked mod's contents, returning the
-- list of top-level mod subdirectories if the mod is valid, throwing
-- an exception if not.
validateMod'
  :: (MonadIO m, MonadThrow m)
  => Path Abs Dir
  -- ^ Location of the unpacked mod
  -> m [Path Rel Dir]
  -- ^ The list of top-level mod subdirectories
validateMod' modDir =
  listDir modDir >>= \case
    ([], []) -> throwM NoContents
    (absDirs, []) ->
      let dirs = map dirname absDirs
          invalid = filter (\x -> not $ Set.member x validTLDs) dirs
      in case invalid of
           (x:_) -> throwM $ InvalidTopLevelDirectory x
           _ -> return $! dirs
    (_, _) -> throwM TopLevelFiles

packMod
  :: (MonadIO m, MonadThrow m)
  => Path Abs Dir
  -- ^ The top-level directory where the mod contents live
  -> Path Abs Dir
  -- ^ The destination directory where the packed mod will be created
  -> String
  -- ^ The mod name/ID
  -> m PackFilePath
  -- ^ The path to the created mod pack
packMod srcDir destDir modName =
  do dirs <- validateMod' srcDir
     tarFileName <- parseRelFile (modName ++ ".fmax")
     let tarPath = destDir </> tarFileName
     createTar tarPath dirs
     return $ PackFilePath tarPath
  where
    createTar :: (MonadIO m) => Path Abs File -> [Path Rel Dir] -> m ()
    createTar tarFile dirs = liftIO $
      do entries <- Tar.pack (toFilePath srcDir) (map toFilePath dirs)
         BL.writeFile (toFilePath tarFile) $ Lzma.compress $ Tar.write entries

unpackMod
  :: (MonadIO m, MonadThrow m)
  => PackFilePath
  -- ^ The mod pack file
  -> Path Abs Dir
  -- ^ Where to unpack the mod pack file
  -> m ()
unpackMod (PackFilePath pf) unpackDir = liftIO $
  do entries <- liftM (Tar.read . Lzma.decompress) $ BL.readFile (toFilePath pf)
     Tar.unpack (toFilePath unpackDir) entries

installMod
  :: (MonadIO m, MonadThrow m, MonadCatch m, MonadMask m)
  => PackFilePath
  -- ^ Location of the packed mod
  -> UserDirPath
  -- ^ The target user directory
  -> m ()
installMod packFile userDir =
  withSystemTempDir "installMod" install
  where
    install :: (MonadIO m, MonadThrow m, MonadCatch m) => Path Abs Dir -> m ()
    install unpackDir =
      do unpackMod packFile unpackDir
         void $ validateMod' unpackDir
         -- Try each pack action, in order.
         --
         -- XXX TODO: should back out of install if an exception
         -- occurs at any step.
         forM_ allActions
               (\action ->
                 let subDir = unpackDir </> packDir action
                 in do exists <- doesDirExist subDir
                       when exists $ go action subDir)
    go :: (MonadIO m, MonadCatch m) => PackAction -> Path Abs Dir -> m ()
    go CreateUserDir subDir =
      do checkUserDir userDir
         copyDirRecur subDir (userDirPath userDir)

-- | Check that the user directory exists by throwing
-- 'NoSuchUserDirectory' if it does not.
checkUserDir :: (MonadThrow m, MonadIO m) => UserDirPath -> m ()
checkUserDir userDir =
  do exists <- doesDirExist (userDirPath userDir)
     unless exists $
       throwM $ NoSuchUserDirectory userDir

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
  deriving (Eq,Typeable)

instance Show ModException where
  show NoContents = "The mod directory is empty"
  show TopLevelFiles = "The mod directory contains top-level files"
  show (InvalidTopLevelDirectory dir) = "The mod directory contains an invalid top-level directory ( " ++ show dir ++ ")"
  show (NoSuchUserDirectory fp) = show fp ++ ": The game user directory doesn't exist"

instance Exception ModException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
