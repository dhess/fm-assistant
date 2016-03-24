{-|
Module      : Game.FMAssistant.Magic
Description : Magic numbers
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

Identify supported file types.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Magic
       ( -- * Identify files
         isZipArchive
       , isRarArchive
       , identifyArchive
          -- * Identify streams
       , isZip
       , isRar
       , identify
         -- * Types
       , Magic(..)
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS (ByteString, length, take)
import qualified Data.ByteString.Streaming as S (ByteString, readFile, toStrict_, take)
import Data.Data
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (foldlWithKey', fromList, keys)
import Path (toFilePath)
import Streaming (runResourceT)

import Game.FMAssistant.Types (ArchiveFilePath(..))

zipMagic :: BS.ByteString
zipMagic = "\x50\x4b\x03\x04"

-- | Returns 'True' if the given byte stream is a ZIP archive.
isZip :: (Monad m) => S.ByteString m r -> m Bool
isZip = magic zipMagic

-- | Returns 'True' if the 'ArchiveFilePath' points to a ZIP file.
isZipArchive :: (MonadIO m) => ArchiveFilePath -> m Bool
isZipArchive (ArchiveFilePath fp) = liftIO $
  runResourceT $ isZip $ S.readFile $ toFilePath fp

rarMagic :: BS.ByteString
rarMagic = "\x52\x61\x72\x21\x1a\x07\x00"

-- | Returns 'True' if the given byte stream is a RAR archive.
isRar :: (Monad m) => S.ByteString m r -> m Bool
isRar = magic rarMagic

-- | Returns 'True' if the 'ArchiveFilePath' points to a RAR file.
isRarArchive :: (MonadIO m) => ArchiveFilePath -> m Bool
isRarArchive (ArchiveFilePath fp) = liftIO $
  runResourceT $ isRar $ S.readFile $ toFilePath fp

magic :: (Monad m) => BS.ByteString -> S.ByteString m r -> m Bool
magic magicString bs =
  -- Using 'toStrict_' here is fine as our magic strings are quite
  -- small and we 'take' before we 'toStrict_'.
  do prefix <- S.toStrict_ $ S.take (fromIntegral $ BS.length magicString) bs
     return $ prefix == magicString

-- | Stream/file types which this module can identify.
data Magic
  = Zip -- ^ ZIP archive
  | Rar -- ^ RAR archive
  deriving (Show,Eq,Data,Typeable)

type MagicMap = Map BS.ByteString Magic

magicMap :: MagicMap
magicMap = Map.fromList [(zipMagic, Zip), (rarMagic, Rar)]

-- | Given a byte stream, attempt to identify it.
identify :: (Monad m) => S.ByteString m r -> m (Maybe Magic)
identify bs =
  let maxLength = foldl' max 0 (map BS.length $ Map.keys magicMap)
  in
    -- See 'isMagic' for reasoning on why 'toStrict_' is ok here.
    do prefix <- S.toStrict_ $ S.take (fromIntegral maxLength) bs
       return $ Map.foldlWithKey' (check prefix) Nothing magicMap
    where
      check _ (Just x) _ _ = Just x
      check prefix _ magicString magicValue
        | magicString == BS.take (BS.length magicString) prefix = Just magicValue
        | otherwise = Nothing

-- | Attempt to identify the file pointed to by the given
-- 'ArchiveFilePathPath'.
identifyArchive :: (MonadIO m) => ArchiveFilePath -> m (Maybe Magic)
identifyArchive (ArchiveFilePath fp) = liftIO $
  runResourceT $ identify $ S.readFile $ toFilePath fp
