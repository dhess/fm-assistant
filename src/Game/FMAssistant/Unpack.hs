{-|
Module      : Game.FMAssistant.Unpack
Description : Utilities for unpacking archives
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Functions for unpacking the various archive types (RAR, ZIP, etc.).

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Unpack
       ( -- * Unpacking actions
         unpackZip
       , unpackRar
         -- * Selecting an unpacking action
       , unpackerFor
       ) where

import Prelude hiding (FilePath)
import Control.Applicative (empty)
import Control.Monad.Managed.Safe (Managed)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (basename, extension)
import Turtle.Format ((%), format, fp)
import Turtle.Prelude (die, procs)

import Game.FMAssistant.Util (mktempdir)

-- | Given the filename of an archive file, use the filename's
-- extension to guess which unarchive action to use, and return that
-- action.
--
-- If, based on the filename's extension, the archive format is
-- unsupported, this function returns 'Nothing'.
unpackerFor :: FilePath -> Maybe (FilePath -> Managed FilePath)
unpackerFor ar
  | Filesystem.extension ar == Just "zip" = Just unpackZip
  | Filesystem.extension ar == Just "ZIP" = Just unpackZip
  | Filesystem.extension ar == Just "rar" = Just unpackRar
  | Filesystem.extension ar == Just "RAR" = Just unpackRar
  | otherwise = Nothing

-- | Unpack a ZIP archive to a temporary directory, whose path is
-- returned.
--
-- N.B. that the temporary directory is 'Managed', so it will be
-- automatically removed when the managed action terminates!
unpackZip :: FilePath -> Managed FilePath
unpackZip zipFile =
  do tmpDir <- mktempdir (format fp $ Filesystem.basename zipFile)
     procs "unzip" [format fp zipFile, "-d", format fp tmpDir] empty
     return tmpDir

-- | Unpack a RAR archive to a temporary directory, whose path is
-- returned.
--
-- N.B. that the temporary directory is 'Managed', so it will be
-- automatically removed when the managed action terminates!
unpackRar :: FilePath -> Managed FilePath
unpackRar rarFile =
  do tmpDir <- mktempdir (format fp $ Filesystem.basename rarFile)
     procs "unrar" ["x", "-v", "-y", "-r", format fp rarFile,  format fp tmpDir] empty
     return tmpDir
