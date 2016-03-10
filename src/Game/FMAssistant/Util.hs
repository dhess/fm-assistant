{-|
Module      : Game.FMAssistant.Util
Description : Utility functions & combinators
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Helpful functions and combinators.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Util
       ( defaultUserDir
       , getHomeDirectory
       , mktempdir
       , fpToHumanText
       , executeQuietly
       ) where

import Prelude hiding (FilePath)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (Managed)
import Data.Text (Text)
import qualified Data.Text as T (intercalate, unpack)
import Filesystem.Path.CurrentOS ((</>), FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString, toText)
import qualified System.Directory as Directory (getHomeDirectory, getTemporaryDirectory)
import System.Exit (ExitCode)
import System.Process.Streaming (execute, exitCode, piped, proc)
import qualified Turtle.Prelude as Turtle (mktempdir)

import Game.FMAssistant.Types (Version(..), versionToFilePath, UserDirFilePath(..))

-- | Construct the default 'UserDirPath' for a given game version.
defaultUserDir :: (MonadIO m) => Version -> m UserDirFilePath
defaultUserDir version =
  do steamDir <- defaultSteamDir
     return $ UserDirFilePath (steamDir </> versionToFilePath version)

-- | A wrapper around 'Directory.getHomeDirectory' which returns the
-- home directory as the proper 'FilePath' type. It's also
-- conveniently wrapped in a 'MonadIO' instance.
getHomeDirectory :: (MonadIO m) => m FilePath
getHomeDirectory = liftIO $ Filesystem.decodeString <$> Directory.getHomeDirectory

-- | A wrapper around 'Turtle.mktempdir' which always uses the system
-- temporary directory as the parent.
mktempdir :: Text -> Managed FilePath
mktempdir template =
  do sysTmpDir <- liftIO $ Filesystem.decodeString <$> Directory.getTemporaryDirectory
     Turtle.mktempdir sysTmpDir template

-- | As 'toText', but strips the 'Either' off the return value and
-- appends "(invalid encoding)" to the resulting text in case 'toText'
-- returns a 'Left' value.
fpToHumanText :: FilePath -> Text
fpToHumanText =
  either (\s -> T.intercalate " " [s, "(invalid encoding)"]) id . Filesystem.toText

-- | Execute an external program and return the 'ExitCode'. Both
-- stderr and stdout are discarded.
executeQuietly
        :: (MonadIO m)
        => Text
        -- ^ Command
        -> [Text]
        -- ^ Arguments
        -> m ExitCode
        -- ^ Exit code
executeQuietly cmd args = liftIO $ execute (piped (proc (T.unpack cmd) (map T.unpack args))) exitCode

-- Not currently exported.
--

-- | The platform-specific Steam directory for the user who is running
-- the 'MonadIO' action.
defaultSteamDir :: (MonadIO m) => m FilePath
defaultSteamDir =
  do homeDir <- getHomeDirectory
     return $ homeDir </> "Documents" </> "Sports Interactive"
