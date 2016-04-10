{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.Repack.KitsSpec
       ( spec
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock (utctDay)
import Path ((</>), filename, mkAbsFile, mkRelFile, parseAbsFile, parseRelFile)
import Path.IO (doesFileExist, getModificationTime, withSystemTempDir)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Mod (PackFilePath(..), unpackMod)
import Game.FMAssistant.Repack.Unpack (UnpackException)
import Game.FMAssistant.Repack.Kits
import Game.FMAssistant.Repack (ArchiveFilePath(..))
import Game.FMAssistant.Util (basename)

getArchiveFilePath :: (MonadIO m) => FilePath -> m ArchiveFilePath
getArchiveFilePath fp = liftIO $
  do fn <- getDataFileName fp
     absfn <- parseAbsFile fn
     return $ ArchiveFilePath absfn

unsupportedFile :: (MonadIO m) => m ArchiveFilePath
unsupportedFile = getArchiveFilePath "data/test/test.tar"

sillyKitsZip :: (MonadIO m) => m ArchiveFilePath
sillyKitsZip = getArchiveFilePath "data/test/Silly kits.zip"

dummyPackV10Zip :: (MonadIO m) => m ArchiveFilePath
dummyPackV10Zip = getArchiveFilePath "data/test/Dummy kit pack v1.0.zip"

malformedPackRar :: (MonadIO m) => m ArchiveFilePath
malformedPackRar = getArchiveFilePath "data/test/Malformed dummy kit pack v1.0.rar"

damagedZipFile :: (MonadIO m) => m ArchiveFilePath
damagedZipFile = getArchiveFilePath "data/test/damaged-test.zip"

anyUnpackException :: Selector UnpackException
anyUnpackException = const True

generateModId :: (MonadIO m) => ArchiveFilePath -> m String
generateModId (ArchiveFilePath archive) =
  let base = basename archive
  in do modifiedTime <- getModificationTime archive
        return $ base ++ "." ++ showGregorian (utctDay modifiedTime)

spec :: Spec
spec =
  do describe "repackKitPack" $
       do it "repacks proper kit packs" $
            withSystemTempDir "KitsSpec" $ \tmpDir ->
              do kitPack1 <- dummyPackV10Zip
                 modPack1 <- repackKitPack kitPack1 tmpDir
                 unpackMod modPack1 tmpDir
                 kitPack2 <- sillyKitsZip
                 modPack2 <- repackKitPack kitPack2 tmpDir
                 unpackMod modPack2 tmpDir
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Dummy kit pack v1.0/config.xml")) `shouldReturn` True
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Dummy kit pack v1.0/flamengo_1.png")) `shouldReturn` True
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Dummy kit pack v1.0/santos_1.png")) `shouldReturn` True
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Silly kits/config.xml")) `shouldReturn` True
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Silly kits/flam_1.png")) `shouldReturn` True
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Silly kits/foo_3.png")) `shouldReturn` True
          it "can handle malformed kit packs" $
            withSystemTempDir "KitsSpec" $ \tmpDir ->
              do kitPack <- malformedPackRar
                 modPack <- repackKitPack kitPack tmpDir
                 unpackMod modPack tmpDir
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Malformed dummy kit pack v1.0/config.xml")) `shouldReturn` True
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Malformed dummy kit pack v1.0/flamengo_1.png")) `shouldReturn` True
                 doesFileExist (tmpDir </> $(mkRelFile "create_user/graphics/kits/Malformed dummy kit pack v1.0/santos_1.png")) `shouldReturn` True
          it "names the repack properly" $
            withSystemTempDir "KitsSpec" $ \tmpDir ->
              do kitPack <- dummyPackV10Zip
                 (PackFilePath modPack) <- repackKitPack kitPack tmpDir
                 modId <- generateModId kitPack
                 parseRelFile (modId ++ ".fmax") `shouldReturn` filename modPack
          it "won't repack a damaged archive file" $
            withSystemTempDir "KitsSpec" $ \tmpDir ->
              do kitPack1 <- damagedZipFile
                 repackKitPack kitPack1 tmpDir `shouldThrow` anyUnpackException
          it "fails if the archive format is unsupported" $
            withSystemTempDir "KitsSpec" $ \tmpDir ->
              do kitPack <- unsupportedFile
                 repackKitPack kitPack tmpDir `shouldThrow` anyUnpackException
          it "fails if the specified archive doesn't exist" $
            withSystemTempDir "KitsSpec" $ \tmpDir ->
              repackKitPack (ArchiveFilePath $(mkAbsFile "/this/doesn't/exist.zip")) tmpDir `shouldThrow` anyIOException
