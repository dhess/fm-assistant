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
       , packName
       , modReplaceUserDir
       , packMod
         -- * Exceptions
       , PackException
       ) where

import qualified Codec.Archive.Tar as Tar (pack, write)
import qualified Codec.Compression.Lzma as Lzma (compress)
import Control.Exception (Exception(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.ByteString.Lazy as BL (writeFile)
import Data.Data
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member)
import Path ((</>), Path, Abs, Rel, Dir, File, dirname, mkRelDir, parseRelFile, toFilePath)
import Path.IO (listDir)

import Game.FMAssistant.Types
       (fmAssistantExceptionToException,
        fmAssistantExceptionFromException)
import Game.FMAssistant.Util (basename)

-- | The type for paths which point to a pack file.
newtype PackFilePath =
  PackFilePath {packFilePath :: Path Abs File}
  deriving (Eq,Show,Ord,Typeable)

-- | Return (as a 'String') the name of an pack file; that is, given a
-- 'PackFilePath', strip off the pathname portion and the trailing
-- file suffix, and return what remains.
--
-- >>> :set -XOverloadedStrings
-- >>> bazPath <- parseAbsFile "/foo/bar/baz.tar"
-- >>> packName $ PackFilePath bazPath
-- "baz"
packName :: PackFilePath -> String
packName = basename . packFilePath

modReplaceUserDir :: Path Rel Dir
modReplaceUserDir = $(mkRelDir "replace_user")

-- | The set of valid top-level directories in a mod pack.
validTLDs :: Set (Path Rel Dir)
validTLDs = Set.fromList [modReplaceUserDir]

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
  listDir srcDir >>= \case
    ([], []) -> throwM NoContents
    (absDirs, []) ->
      let dirs = map dirname absDirs
          invalid = filter (\x -> not $ Set.member x validTLDs) dirs
      in case invalid of
           (x:_) -> throwM $ InvalidTopLevelDirectory x
           _ ->
             do tarFileName <- parseRelFile (modName ++ ".fmax")
                let tarPath = destDir </> tarFileName
                createTar tarPath dirs
                return $ PackFilePath tarPath
    (_, _) -> throwM TopLevelFiles
  where
    createTar :: (MonadIO m) => Path Abs File -> [Path Rel Dir] -> m ()
    createTar tarFile dirs = liftIO $
      do entries <- Tar.pack (toFilePath srcDir) (map toFilePath dirs)
         BL.writeFile (toFilePath tarFile) $ Lzma.compress $ Tar.write entries

-- | Exceptions which can occur while packing a mod.
data PackException
  = NoContents
    -- ^ The mod directory is empty
  | TopLevelFiles
    -- ^ The mod directory contains top-level files
  | InvalidTopLevelDirectory (Path Rel Dir)
    -- ^ The mod directory contains an invalid top-level directory
  deriving (Eq,Typeable)

instance Show PackException where
  show NoContents = "The mod directory is empty"
  show TopLevelFiles = "The mod directory contains top-level files"
  show (InvalidTopLevelDirectory dir) = "The mod directory contains an invalid top-level directory ( " ++ show dir ++ ")"

instance Exception PackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
