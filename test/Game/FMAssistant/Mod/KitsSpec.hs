{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.Mod.KitsSpec
       ( spec
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Path ((</>), mkAbsFile, mkRelDir, mkRelFile, parseAbsFile)
import Path.IO (doesFileExist, withSystemTempDir)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Install (InstallConfig(..), InstallException, installMod, replaceMod)
import Game.FMAssistant.Mod.Kits
import Game.FMAssistant.Types (ArchiveFilePath(..), UserDirPath(..))
import Game.FMAssistant.Unpack (UnpackException)

withTmpUserDir :: (UserDirPath -> IO a) -> IO a
withTmpUserDir action = withSystemTempDir "KitSpec" $ \dir ->
  action (UserDirPath dir)

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

dummyPackV11Zip :: (MonadIO m) => m ArchiveFilePath
dummyPackV11Zip = getArchiveFilePath "data/test/Dummy kit pack v1.1.zip"

malformedPackZip :: (MonadIO m) => m ArchiveFilePath
malformedPackZip = getArchiveFilePath "data/test/Malformed dummy kit pack v1.0.zip"

dummyPackV10Rar :: (MonadIO m) => m ArchiveFilePath
dummyPackV10Rar = getArchiveFilePath "data/test/Dummy kit pack v1.0.rar"

malformedPackRar :: (MonadIO m) => m ArchiveFilePath
malformedPackRar = getArchiveFilePath "data/test/Malformed dummy kit pack v1.0.rar"

damagedZipFile :: (MonadIO m) => m ArchiveFilePath
damagedZipFile = getArchiveFilePath "data/test/damaged-test.zip"

damagedRarFile :: (MonadIO m) => m ArchiveFilePath
damagedRarFile = getArchiveFilePath "data/test/damaged-test.rar"

anyUnpackException :: Selector UnpackException
anyUnpackException = const True

anyInstallException :: Selector InstallException
anyInstallException = const True

spec :: Spec
spec =
  do describe "validateKitPack" $
       do it "validates proper kit packs" $
            do (dummyPackV10Zip >>= validateKitPack) `shouldReturn` ()
               (dummyPackV10Rar >>= validateKitPack) `shouldReturn` ()
          it "validates malformed kit packs (they'll be patched up during installation)" $
            do (malformedPackZip >>= validateKitPack) `shouldReturn` ()
               (malformedPackRar >>= validateKitPack) `shouldReturn` ()
          it "reports invalid archives" $
            do (damagedZipFile >>= validateKitPack) `shouldThrow` anyUnpackException
               (damagedRarFile >>= validateKitPack) `shouldThrow` anyUnpackException
          it "reports unsupported archive types" $
            (unsupportedFile >>= validateKitPack) `shouldThrow` anyUnpackException
     describe "installKitPack" $
       do it "installs kit packs in the user directory's kit path" $
            withTmpUserDir $ \dir ->
              let kitDir = userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do runReaderT
                     (do dummyPackV10Zip >>= installKitPack
                         sillyKitsZip >>= installKitPack)
                     (InstallConfig dir installMod)
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/santos_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/flam_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/foo_3.png")) `shouldReturn` True
          it "won't overwrite an existing install when run with installMod" $
            withTmpUserDir $ \dir ->
              do runReaderT (dummyPackV10Zip >>= installKitPack) (InstallConfig dir installMod)
                 runReaderT (dummyPackV11Zip >>= installKitPack) (InstallConfig dir installMod) `shouldThrow` anyIOException
          it "is (effectively) idempotent when run with replaceMod" $
            withTmpUserDir $ \dir ->
              let kitDir = userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do runReaderT (dummyPackV10Zip >>= installKitPack) (InstallConfig dir installMod)
                   runReaderT (dummyPackV10Rar >>= installKitPack) (InstallConfig dir replaceMod)
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/santos_1.png")) `shouldReturn` True
          it "when run with replaceMod, removes any existing version when installing a new version" $
            withTmpUserDir $ \dir ->
              let kitDir = userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do runReaderT
                     (do dummyPackV10Zip >>= installKitPack
                         dummyPackV11Zip >>= installKitPack)
                     (InstallConfig dir replaceMod)
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/santos_1.png")) `shouldReturn` False
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/vasco_1.png")) `shouldReturn` True
          it "doesn't remove other kits when installing a new version" $
            withTmpUserDir $ \dir ->
              let kitDir = userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do runReaderT
                     (do dummyPackV10Zip >>= installKitPack
                         sillyKitsZip >>= installKitPack
                         dummyPackV11Zip >>= installKitPack)
                     (InstallConfig dir replaceMod)
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/flam_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/foo_3.png")) `shouldReturn` True
          it "installs a malformed ZIP'ed kit pack using the pack's name as a top-level directory" $
            withTmpUserDir $ \dir ->
              let kitDir = userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do runReaderT (malformedPackZip >>= installKitPack) (InstallConfig dir installMod)
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/santos_1.png")) `shouldReturn` True
          it "installs a malformed RAR'ed kit pack using the pack's name as a top-level directory" $
            withTmpUserDir $ \dir ->
              let kitDir = userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do runReaderT (malformedPackRar >>= installKitPack) (InstallConfig dir installMod)
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/santos_1.png")) `shouldReturn` True
          it "won't install a damaged archive file" $
            withTmpUserDir $ \dir ->
              do runReaderT (damagedZipFile >>= installKitPack) (InstallConfig dir installMod) `shouldThrow` anyUnpackException
                 runReaderT (damagedRarFile >>= installKitPack) (InstallConfig dir installMod) `shouldThrow` anyUnpackException
          it "fails if the archive format is unsupported" $
            withTmpUserDir $ \dir ->
              runReaderT (unsupportedFile >>= installKitPack) (InstallConfig dir installMod) `shouldThrow` anyUnpackException
          it "fails if the specified archive doesn't exist" $
            withTmpUserDir $ \dir ->
              runReaderT (installKitPack (ArchiveFilePath $(mkAbsFile "/this/doesn't/exist.zip"))) (InstallConfig dir installMod) `shouldThrow` anyIOException
          it "fails if the user directory doesn't exist" $
            withTmpUserDir $ \dir ->
              let missingUserDir = UserDirPath $ userDirPath dir </> $(mkRelDir "missing")
              in
                runReaderT (dummyPackV10Zip >>= installKitPack) (InstallConfig missingUserDir installMod) `shouldThrow` anyInstallException
