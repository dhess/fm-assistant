{-|
Module      : Game.FMAssistant.Mod.Kits
Description : A monad for installing mods
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Install
       ( -- * MonadInstall class
         MonadInstall(..)
       , InstallModT(..)
       , InstallMod
       , runInstallMod
       , ReplaceModT(..)
       , ReplaceMod
       , runReplaceMod
         -- * Installation actions
         --
         -- In addition to "running" one of the 'MonadInstall'
         -- instances, you can also run several installation actions
         -- in a 'MonadIO' context.
       , installMod
       , replaceMod
       ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, onException, throwM)
import Control.Monad.Cont (MonadCont, ContT)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Resource (release, runResourceT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)
import Control.Monad.Writer (MonadWriter)
import Path ((</>), Path, Abs, Rel, Dir, dirname, parent, toFilePath)
import Path.IO (doesDirExist, removeDirRecur, renameDir)
import System.FilePath (dropTrailingPathSeparator)
import System.IO.Error (alreadyExistsErrorType, mkIOError)

import Game.FMAssistant.Util (createTempDir)

-- | A monad which provides a context for installing mods.
class (Monad m) => MonadInstall m where
  -- | Install a mod.
  install
    :: Path Abs Dir
    -- ^ The directory containing the mod contents
    -> Path Abs Dir
    -- ^ The (parent) directory into which the mod will be installed
    -> m ()

instance (MonadInstall m) => MonadInstall (IdentityT m) where
  install src dst = lift $ install src dst

instance (MonadInstall m) => MonadInstall (ContT r m) where
  install src dst = lift $ install src dst

instance (MonadInstall m) => MonadInstall (ExceptT e m) where
  install src dst = lift $ install src dst

instance (MonadInstall m) => MonadInstall (ListT m) where
  install src dst = lift $ install src dst

instance (MonadInstall m) => MonadInstall (MaybeT m) where
  install src dst = lift $ install src dst

instance (MonadInstall m) => MonadInstall (ReaderT r m) where
  install src dst = lift $ install src dst

instance (MonadInstall m, Monoid w) => MonadInstall (LazyRWS.RWST r w s m) where
  install src dst = lift $ install src dst

instance (MonadInstall m, Monoid w) => MonadInstall (StrictRWS.RWST r w s m) where
  install src dst = lift $ install src dst

instance (MonadInstall m) => MonadInstall (LazyState.StateT s m) where
  install src dst = lift $ install src dst

instance (MonadInstall m) => MonadInstall (StrictState.StateT s m) where
  install src dst = lift $ install src dst

instance (MonadInstall m, Monoid w) => MonadInstall (LazyWriter.WriterT w m) where
  install src dst = lift $ install src dst

instance (MonadInstall m, Monoid w) => MonadInstall (StrictWriter.WriterT w m) where
  install src dst = lift $ install src dst

-- | An instance of 'MonadInstall' which will not overwrite an
-- existing installation of a mod with the same name.
--
-- See 'installMod' for details.
newtype InstallModT m a =
  InstallModT {runInstallModT :: m a}
  deriving (Functor,Alternative,Applicative,Monad,MonadFix,MonadPlus,MonadThrow,MonadCatch,MonadMask,MonadCont,MonadIO,MonadReader r,MonadError e,MonadWriter w,MonadState s,MonadRWS r w s)

instance MonadTrans InstallModT where
  lift = InstallModT

instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadInstall (InstallModT m) where
  install = installMod

-- | An 'InstallModT' transformer specialized to 'IO'.
type InstallMod = InstallModT IO

-- | Run an 'InstallMod' action in the 'IO' monad.
runInstallMod :: InstallMod a -> IO a
runInstallMod = runInstallModT

-- | An instance of 'MonadInstall' which will replace an existing
-- installation of a mod, or just install it if no existing
-- installation is present.
--
-- See 'replaceMod' for details.
newtype ReplaceModT m a =
  ReplaceModT {runReplaceModT :: m a}
  deriving (Functor,Alternative,Applicative,Monad,MonadFix,MonadPlus,MonadThrow,MonadCatch,MonadMask,MonadCont,MonadIO,MonadReader r,MonadError e,MonadWriter w,MonadState s,MonadRWS r w s)

instance MonadTrans ReplaceModT where
  lift = ReplaceModT

instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadInstall (ReplaceModT m) where
  install = replaceMod

-- | A 'ReplaceModT' transformer specialized to 'IO'.
type ReplaceMod = ReplaceModT IO

-- | Run a 'ReplaceMod' action in the 'IO' monad.
runReplaceMod :: ReplaceMod a -> IO a
runReplaceMod = runReplaceModT

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
  :: (MonadIO m, MonadThrow m, MonadCatch m)
  => Path Abs Dir
  -- ^ The directory containing the mod contents
  -> Path Abs Dir
  -- ^ The (parent) directory into which the mod will be installed
  -> m ()
installMod srcPath dstPath =
  let targetPath :: Path Abs Dir
      targetPath = dstPath </> dirname srcPath
  in do targetExists <- doesDirExist targetPath
        when targetExists $
          throwM $
            mkIOError alreadyExistsErrorType
                      "installMod"
                      Nothing
                      (Just $ toFilePath targetPath)
        moveAtomically srcPath targetPath

-- | Install a mod, replacing an existing installation, if present.
--
-- (Despite the misleading name, this action will also just install
-- the mod if an existing installation is not present.)
--
-- If, during installation, an error occurs, the partially-installed
-- mod will be removed before the error is reported, and the existing
-- installation will be restored.
replaceMod
  :: (MonadIO m, MonadThrow m, MonadCatch m)
  => Path Abs Dir
  -- ^ The directory containing the mod contents
  -> Path Abs Dir
  -- ^ The (parent) directory into which the mod will be installed
  -> m ()
replaceMod srcPath dstPath =
  let targetPath :: Path Abs Dir
      targetPath = dstPath </> dirname srcPath
  in do targetExists <- doesDirExist targetPath
        if targetExists
           then replaceAtomically targetPath srcPath
           else moveAtomically srcPath targetPath

-- Helper functions. These are not exported.
--

-- | All or nothing directory move.
moveAtomically
  :: (MonadIO m, MonadCatch m)
  => Path Abs Dir
  -- ^ Source
  -> Path Abs Dir
  -- ^ Destination
  -> m ()
moveAtomically src dst =
  onException
    (renameDir src dst)
    (do cleanup <- doesDirExist dst
        when cleanup $ removeDirRecur dst)

-- | All or nothing directory replacement.
replaceAtomically
  :: (MonadIO m, MonadCatch m)
  => Path Abs Dir
  -- ^ Path to existing directory
  -> Path Abs Dir
  -- ^ Path to replacement directory
  -> m ()
replaceAtomically existingPath replacementPath =
  let parentDir :: Path Abs Dir
      parentDir = parent existingPath
      dirName :: Path Rel Dir
      dirName = dirname existingPath
  in liftIO $
     runResourceT $
     do (rkey,backupDir) <-
          createTempDir parentDir
                        (dropTrailingPathSeparator $ toFilePath dirName)
        moveAtomically existingPath backupDir
        let backupPath = backupDir </> dirName
        onException (moveAtomically replacementPath existingPath)
                    (renameDir backupPath existingPath)
        release rkey
