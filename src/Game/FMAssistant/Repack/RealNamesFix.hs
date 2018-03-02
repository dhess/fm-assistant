{-|
Module      : Game.FMAssistant.Repack.RealNamesFix
Description : Things in common with all RNF versions
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

module Game.FMAssistant.Repack.RealNamesFix
  ( -- * Common functions
    dbSubDir
  , dbcSubDir
  , editorDataSubDir
  , edtSubDir
  , lncSubDir
    -- * Exceptions
    -- * Exceptions
  , RealNamesFixRepackException(..)
  ) where

import Control.Exception (Exception(..))
import Data.Data
import Path ((</>), Path, Rel, Dir, mkRelDir)

import Game.FMAssistant.Repack.Internal (ArchiveFilePath(..))
import Game.FMAssistant.Types
  ( Version(..)
  , fmAssistantExceptionFromException
  , fmAssistantExceptionToException
  )

dbSubDir :: Version -> Path Rel Dir -> Path Rel Dir
dbSubDir FM18 dbVersionDir = $(mkRelDir "data/database/db") </> dbVersionDir
dbSubDir _ dbVersionDir = $(mkRelDir "data/db") </> dbVersionDir

dbcSubDir :: Version -> Path Rel Dir -> Path Rel Dir
dbcSubDir version dbVersionDir = dbSubDir version dbVersionDir </> $(mkRelDir "dbc")

edtSubDir :: Version -> Path Rel Dir -> Path Rel Dir
edtSubDir version dbVersionDir = dbSubDir version dbVersionDir </> $(mkRelDir "edt")

lncSubDir :: Version -> Path Rel Dir -> Path Rel Dir
lncSubDir version dbVersionDir = dbSubDir version dbVersionDir </> $(mkRelDir "lnc")

editorDataSubDir :: Path Rel Dir
editorDataSubDir = $(mkRelDir "editor data")

data RealNamesFixRepackException
  = MissingDbcDir ArchiveFilePath
    -- ^ The archive is missing the "dbc" directory
  | MissingEdtDir ArchiveFilePath
    -- ^ The archive is missing the "edt" directory
  | MissingLncDir ArchiveFilePath
    -- ^ The archive is missing the "lnc" directory
  | MissingRnfEditorDataDir ArchiveFilePath
    -- ^ The archive is missing the "editor data" directory
  | MissingRealNamesFixDir ArchiveFilePath
    -- ^ The archive is missing the Real Names Fix directory
  deriving (Eq,Typeable)

instance Show RealNamesFixRepackException where
  show (MissingDbcDir fp) = show fp ++ ": Malformed Real Names fix mod (no \"dbc\" directory)"
  show (MissingEdtDir fp) = show fp ++ ": Malformed Real Names Fix mod (no \"edt\" directory)"
  show (MissingLncDir fp) = show fp ++ ": Malformed Real Names Fix mod (no \"lnc\" directory)"
  show (MissingRnfEditorDataDir fp) = show fp ++ ": Malformed Real Names Fix mod (no \"editor data\" directory)"
  show (MissingRealNamesFixDir fp) = show fp ++ ": Malformed Real Names Fix mod (no \"Real Names Fix\" directory)"

instance Exception RealNamesFixRepackException where
  toException = fmAssistantExceptionToException
  fromException = fmAssistantExceptionFromException
