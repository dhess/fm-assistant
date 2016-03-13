{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.Mod.KitsSpec
       ( spec
       ) where

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Types (ArchiveFilePath(..), UserDirFilePath(..))
import Game.FMAssistant.Mod.Kits

withTmpUserDir :: (UserDirFilePath -> IO a) -> IO a
withTmpUserDir action = withSystemTempDirectory "KitSpec" $ \dir ->
  action (UserDirFilePath dir)

unsupportedFile :: IO ArchiveFilePath
unsupportedFile = ArchiveFilePath <$> getDataFileName "data/test/test.tar"

sillyKitsZip :: IO ArchiveFilePath
sillyKitsZip = ArchiveFilePath <$> getDataFileName "data/test/Silly kits.zip"

dummyPackV10Zip :: IO ArchiveFilePath
dummyPackV10Zip = ArchiveFilePath <$> getDataFileName "data/test/Dummy kit pack v1.0.zip"

dummyPackV11Zip :: IO ArchiveFilePath
dummyPackV11Zip = ArchiveFilePath <$> getDataFileName "data/test/Dummy kit pack v1.1.zip"

malformedPackZip :: IO ArchiveFilePath
malformedPackZip = ArchiveFilePath <$> getDataFileName "data/test/Malformed dummy kit pack v1.0.zip"

dummyPackV10Rar :: IO ArchiveFilePath
dummyPackV10Rar = ArchiveFilePath <$> getDataFileName "data/test/Dummy kit pack v1.0.rar"

malformedPackRar :: IO ArchiveFilePath
malformedPackRar = ArchiveFilePath <$> getDataFileName "data/test/Malformed dummy kit pack v1.0.rar"

damagedZipFile :: IO ArchiveFilePath
damagedZipFile = ArchiveFilePath <$> getDataFileName "data/test/damaged-test.zip"

damagedRarFile :: IO ArchiveFilePath
damagedRarFile = ArchiveFilePath <$> getDataFileName "data/test/damaged-test.rar"

anyKitPackException :: Selector KitPackException
anyKitPackException = const True

spec :: Spec
spec =
  do describe "validateKitPack" $
       do it "validates proper kit packs" $
            do (dummyPackV10Zip >>= validateKitPack) `shouldReturn` ()
               (dummyPackV10Rar >>= validateKitPack) `shouldReturn` ()
          it "reports malformed kit packs" $
            do (malformedPackZip >>= validateKitPack) `shouldThrow` anyKitPackException
               (malformedPackRar >>= validateKitPack) `shouldThrow` anyKitPackException
          it "reports invalid archives" $
            do (damagedZipFile >>= validateKitPack) `shouldThrow` anyKitPackException
               (damagedRarFile >>= validateKitPack) `shouldThrow` anyKitPackException
          it "reports unsupported archive types" $
            (unsupportedFile >>= validateKitPack) `shouldThrow` anyKitPackException
     describe "installKitPack" $
       do it "installs kit packs in the user directory's kit path" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirFilePath dir </> "graphics" </> "kits"
              in
                do dummyPackV10Zip >>= installKitPack dir
                   sillyKitsZip >>= installKitPack dir
                   doesFileExist (kitDir </> "Dummy kit pack" </> "config.xml") `shouldReturn` True
                   doesFileExist (kitDir </> "Dummy kit pack" </> "flamengo_1.png") `shouldReturn` True
                   doesFileExist (kitDir </> "Dummy kit pack" </> "santos_1.png") `shouldReturn` True
                   doesFileExist (kitDir </> "Silly kits" </> "config.xml") `shouldReturn` True
                   doesFileExist (kitDir </> "Silly kits" </> "flam_1.png") `shouldReturn` True
                   doesFileExist (kitDir </> "Silly kits" </> "foo_3.png") `shouldReturn` True
          it "is (effectively) idempotent" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirFilePath dir </> "graphics" </> "kits"
              in
                do dummyPackV10Zip >>= installKitPack dir
                   dummyPackV10Rar >>= installKitPack dir
                   doesFileExist (kitDir </> "Dummy kit pack" </> "config.xml") `shouldReturn` True
                   doesFileExist (kitDir </> "Dummy kit pack" </> "flamengo_1.png") `shouldReturn` True
                   doesFileExist (kitDir </> "Dummy kit pack" </> "santos_1.png") `shouldReturn` True
          it "removes any existing version when installing a new version" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirFilePath dir </> "graphics" </> "kits"
              in
                do dummyPackV10Zip >>= installKitPack dir
                   dummyPackV11Zip >>= installKitPack dir
                   doesFileExist (kitDir </> "Dummy kit pack" </> "config.xml") `shouldReturn` True
                   doesFileExist (kitDir </> "Dummy kit pack" </> "flamengo_1.png") `shouldReturn` True
                   doesFileExist (kitDir </> "Dummy kit pack" </> "santos_1.png") `shouldReturn` False
                   doesFileExist (kitDir </> "Dummy kit pack" </> "vasco_1.png") `shouldReturn` True
          it "doesn't remove other kits when installing a new version" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirFilePath dir </> "graphics" </> "kits"
              in
                do dummyPackV10Zip >>= installKitPack dir
                   sillyKitsZip >>= installKitPack dir
                   dummyPackV11Zip >>= installKitPack dir
                   doesFileExist (kitDir </> "Silly kits" </> "config.xml") `shouldReturn` True
                   doesFileExist (kitDir </> "Silly kits" </> "flam_1.png") `shouldReturn` True
                   doesFileExist (kitDir </> "Silly kits" </> "foo_3.png") `shouldReturn` True
          it "won't install a malformed pack" $
            withTmpUserDir $ \dir ->
              (malformedPackRar >>= installKitPack dir) `shouldThrow` anyKitPackException
          it "won't install a damaged archive file" $
            withTmpUserDir $ \dir ->
              do (damagedZipFile >>= installKitPack dir) `shouldThrow` anyKitPackException
                 (damagedRarFile >>= installKitPack dir) `shouldThrow` anyKitPackException
          it "fails if the archive format is unsupported" $
            withTmpUserDir $ \dir ->
              (unsupportedFile >>= installKitPack dir) `shouldThrow` anyKitPackException
          it "fails if the specified archive doesn't exist" $
            withTmpUserDir $ \dir ->
              installKitPack dir (ArchiveFilePath "/this/doesn't/exist.zip") `shouldThrow` anyIOException
          it "fails if the user directory doesn't exist" $
            withTmpUserDir $ \dir ->
              let missingUserDir = UserDirFilePath $ _userDirFilePath dir </> "missing"
              in
                (dummyPackV10Zip >>= installKitPack missingUserDir) `shouldThrow` anyKitPackException
