{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.Mod.KitsSpec
       ( spec
       ) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS ((</>), FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString, encodeString)
import qualified System.Directory as Directory (doesDirectoryExist, doesFileExist, getTemporaryDirectory)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Types (ArchiveFilePath(..), UserDirFilePath(..), SomeFMAssistantException)
import Game.FMAssistant.Unpack (UnpackException(..))
import Game.FMAssistant.Mod.Kits

systemTmpDir :: IO FilePath
systemTmpDir = Filesystem.decodeString <$> Directory.getTemporaryDirectory

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist fp = Directory.doesDirectoryExist (Filesystem.encodeString fp)

doesFileExist :: FilePath -> IO Bool
doesFileExist fp = Directory.doesFileExist (Filesystem.encodeString fp)

unsupportedFile :: IO ArchiveFilePath
unsupportedFile = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/test.tar"

dummyPackV10Zip :: IO ArchiveFilePath
dummyPackV10Zip = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/Dummy kit pack v1.0.zip"

dummyPackV11Zip :: IO ArchiveFilePath
dummyPackV11Zip = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/Dummy kit pack v1.1.zip"

malformedPackZip :: IO ArchiveFilePath
malformedPackZip = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/Malformed dummy kit pack v1.0.zip"

dummyPackV10Rar :: IO ArchiveFilePath
dummyPackV10Rar = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/Dummy kit pack v1.0.rar"

dummyPackV11Rar :: IO ArchiveFilePath
dummyPackV11Rar = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/Dummy kit pack v1.1.rar"

malformedPackRar :: IO ArchiveFilePath
malformedPackRar = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/Malformed dummy kit pack v1.0.rar"

damagedZipFile :: IO ArchiveFilePath
damagedZipFile = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/damaged-test.zip"

damagedRarFile :: IO ArchiveFilePath
damagedRarFile = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/damaged-test.rar"

anyUnpackException :: Selector UnpackException
anyUnpackException = const True

anyKitPackException :: Selector KitPackException
anyKitPackException = const True

anyFMAssistantException :: Selector SomeFMAssistantException
anyFMAssistantException = const True

spec :: Spec
spec =
  do describe "kitPath" $
       it "constructs 'KitPath's from 'UserDirFilePath's" $
         do filePath (kitPath (UserDirFilePath "/foo/bar")) `shouldBe` "/foo/bar/graphics/kits"
            filePath (kitPath (UserDirFilePath "baz/qux/")) `shouldBe` "baz/qux/graphics/kits"
     describe "validateKitPack" $
      do it "validates proper kit packs" $
           do (dummyPackV10Zip >>= validateKitPack) `shouldReturn` ()
              (dummyPackV10Rar >>= validateKitPack) `shouldReturn` ()
         it "reports malformed kit packs" $
           do (malformedPackZip >>= validateKitPack) `shouldThrow` anyKitPackException
              (malformedPackRar >>= validateKitPack) `shouldThrow` anyKitPackException
         it "reports invalid archives" $
           do (damagedZipFile >>= validateKitPack) `shouldThrow` anyUnpackException
              (damagedRarFile >>= validateKitPack) `shouldThrow` anyUnpackException
         it "reports unsupported archive types" $
           do (unsupportedFile >>= validateKitPack) `shouldThrow` anyUnpackException
