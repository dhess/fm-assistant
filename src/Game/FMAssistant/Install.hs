{-|
Module      : Game.FMAssistant.Mod.Kits
Description : A monad for installing mods
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Install
       ( -- * Installation config
         InstallConfig(..)
       , HasInstallConfig(..)
         -- * Installation actions
       , InstallAction
       , install
       , installMod
       , replaceMod
         -- * Installation utility functions
       , userDirExists
       ) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadThrow, onException, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Resource (release, runResourceT)
import Path ((</>), Path, Abs, Rel, Dir, dirname, parent, toFilePath)
import Path.IO (doesDirExist, removeDirRecur, renameDir)
import System.FilePath (dropTrailingPathSeparator)
import System.IO.Error (alreadyExistsErrorType, mkIOError)

import Game.FMAssistant.Util (createTempDir)
import Game.FMAssistant.Types (UnpackDirPath(..), UserDirPath(..))

type InstallAction = UnpackDirPath -> Path Abs Dir -> IO ()

-- | An installer config.
data InstallConfig =
  InstallConfig {_userDir :: !UserDirPath
                ,_installer :: InstallAction}

makeClassy ''InstallConfig

-- | Install a mod using an 'InstallConfig'.
--
-- If, during installation, an error occurs, the partially-installed
-- mod will be removed from the installation directory before the
-- error is reported.
install :: (MonadIO m, MonadThrow m, MonadCatch m, MonadReader r m, HasInstallConfig r) => UnpackDirPath -> Path Rel Dir -> m ()
install srcPath dstPath =
  do f <- view installer
     udir <- userDirPath <$> view userDir
     liftIO $ f srcPath (udir </> dstPath)

-- | Install a mod, but do not overwrite an existing installation, if
-- present.
--
-- The action will throw an 'IOError' with
-- 'System.IO.Error.IOErrorType' 'alreadyExistsErrorType' if an
-- existing installation is detected.
--
-- If, during installation, an error occurs, the partially-installed
-- mod will be removed from the installation directory before the
-- error is reported.
installMod
  :: UnpackDirPath
  -- ^ The directory containing the mod contents
  -> Path Abs Dir
  -- ^ The directory where the mod contents will be installed
  -> IO ()
installMod (UnpackDirPath srcPath) dstPath =
  do targetExists <- doesDirExist dstPath
     when targetExists $
       throwM $
         mkIOError alreadyExistsErrorType
                   "installMod"
                   Nothing
                   (Just $ toFilePath dstPath)
     moveAtomically srcPath dstPath

-- | Install a mod, replacing an existing installation, if present.
--
-- (Despite the misleading name, this action will also just install
-- the mod if an existing installation is not present.)
--
-- If, during installation, an error occurs, the partially-installed
-- mod will be removed before the error is reported, and the existing
-- installation will be restored.
replaceMod
  :: UnpackDirPath
  -- ^ The directory containing the mod contents
  -> Path Abs Dir
  -- ^ The directory where the mod contents will be installed
  -> IO ()
replaceMod (UnpackDirPath srcPath) dstPath =
  do targetExists <- doesDirExist dstPath
     if targetExists
        then replaceAtomically dstPath srcPath
        else moveAtomically srcPath dstPath

-- | Does the user directory specified by the config exist?
userDirExists :: (MonadIO m, MonadReader r m, HasInstallConfig r) => m Bool
userDirExists =
  do udir <- userDirPath <$> view userDir
     doesDirExist udir

-- Helper functions. These are not exported.
--

-- | All or nothing directory move.
moveAtomically
  :: Path Abs Dir
  -- ^ Source
  -> Path Abs Dir
  -- ^ Destination
  -> IO ()
moveAtomically src dst =
  onException
    (renameDir src dst)
    (do cleanup <- doesDirExist dst
        when cleanup $ removeDirRecur dst)

-- | All or nothing directory replacement.
replaceAtomically
  :: Path Abs Dir
  -- ^ Path to existing directory
  -> Path Abs Dir
  -- ^ Path to replacement directory
  -> IO ()
replaceAtomically existingPath replacementPath =
  let parentDir :: Path Abs Dir
      parentDir = parent existingPath
      dirName :: Path Rel Dir
      dirName = dirname existingPath
  in runResourceT $
       do (rkey,backupDir) <-
            createTempDir parentDir
                          (dropTrailingPathSeparator $ toFilePath dirName)
          liftIO $ moveAtomically existingPath backupDir
          let backupPath = backupDir </> dirName
          onException (liftIO $ moveAtomically replacementPath existingPath)
                      (renameDir backupPath existingPath)
          release rkey
