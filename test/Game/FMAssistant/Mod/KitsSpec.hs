{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.Mod.KitsSpec
       ( spec
       ) where

import Path ((</>), mkAbsFile, mkRelDir, mkRelFile, parseAbsFile)
import Path.IO (doesFileExist, withSystemTempDir)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Install (runInstallMod, runReplaceMod)
import Game.FMAssistant.Mod.Kits
import Game.FMAssistant.Types (ArchiveFilePath(..), UserDirPath(..))

withTmpUserDir :: (UserDirPath -> IO a) -> IO a
withTmpUserDir action = withSystemTempDir "KitSpec" $ \dir ->
  action (UserDirPath dir)

getArchiveFilePath :: FilePath -> IO ArchiveFilePath
getArchiveFilePath fp =
  do fn <- getDataFileName fp
     absfn <- parseAbsFile fn
     return $ ArchiveFilePath absfn

unsupportedFile :: IO ArchiveFilePath
unsupportedFile = getArchiveFilePath "data/test/test.tar"

sillyKitsZip :: IO ArchiveFilePath
sillyKitsZip = getArchiveFilePath "data/test/Silly kits.zip"

dummyPackV10Zip :: IO ArchiveFilePath
dummyPackV10Zip = getArchiveFilePath "data/test/Dummy kit pack v1.0.zip"

dummyPackV11Zip :: IO ArchiveFilePath
dummyPackV11Zip = getArchiveFilePath "data/test/Dummy kit pack v1.1.zip"

malformedPackZip :: IO ArchiveFilePath
malformedPackZip = getArchiveFilePath "data/test/Malformed dummy kit pack v1.0.zip"

dummyPackV10Rar :: IO ArchiveFilePath
dummyPackV10Rar = getArchiveFilePath "data/test/Dummy kit pack v1.0.rar"

malformedPackRar :: IO ArchiveFilePath
malformedPackRar = getArchiveFilePath "data/test/Malformed dummy kit pack v1.0.rar"

damagedZipFile :: IO ArchiveFilePath
damagedZipFile = getArchiveFilePath "data/test/damaged-test.zip"

damagedRarFile :: IO ArchiveFilePath
damagedRarFile = getArchiveFilePath "data/test/damaged-test.rar"

anyKitPackException :: Selector KitPackException
anyKitPackException = const True

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
            do (damagedZipFile >>= validateKitPack) `shouldThrow` anyKitPackException
               (damagedRarFile >>= validateKitPack) `shouldThrow` anyKitPackException
          it "reports unsupported archive types" $
            (unsupportedFile >>= validateKitPack) `shouldThrow` anyKitPackException
     describe "installKitPack" $
       do it "installs kit packs in the user directory's kit path" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do dummyPackV10Zip >>= runInstallMod . installKitPack dir
                   sillyKitsZip >>= runInstallMod . installKitPack dir
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/santos_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/flam_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/foo_3.png")) `shouldReturn` True
          it "won't overwrite an existing install when run with runInstallMod" $
            withTmpUserDir $ \dir ->
              do dummyPackV10Zip >>= runInstallMod . installKitPack dir
                 (dummyPackV11Zip >>= runInstallMod . installKitPack dir) `shouldThrow` anyIOException
          it "is (effectively) idempotent when run with runReplaceMod" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do dummyPackV10Zip >>= runInstallMod . installKitPack dir
                   dummyPackV10Rar >>= runReplaceMod . installKitPack dir
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/santos_1.png")) `shouldReturn` True
          it "when run with runReplaceMod, removes any existing version when installing a new version" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do dummyPackV10Zip >>= runInstallMod . installKitPack dir
                   dummyPackV11Zip >>= runReplaceMod . installKitPack dir
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/santos_1.png")) `shouldReturn` False
                   doesFileExist (kitDir </> $(mkRelFile "Dummy kit pack/vasco_1.png")) `shouldReturn` True
          it "doesn't remove other kits when installing a new version" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do dummyPackV10Zip >>= runInstallMod . installKitPack dir
                   sillyKitsZip >>= runInstallMod . installKitPack dir
                   dummyPackV11Zip >>= runReplaceMod . installKitPack dir
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/flam_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Silly kits/foo_3.png")) `shouldReturn` True
          it "installs a malformed ZIP'ed kit pack using the pack's name as a top-level directory" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do malformedPackZip >>= runInstallMod . installKitPack dir
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/santos_1.png")) `shouldReturn` True
          it "installs a malformed RAR'ed kit pack using the pack's name as a top-level directory" $
            withTmpUserDir $ \dir ->
              let kitDir = _userDirPath dir </> $(mkRelDir "graphics/kits")
              in
                do malformedPackRar >>= runInstallMod . installKitPack dir
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/config.xml")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/flamengo_1.png")) `shouldReturn` True
                   doesFileExist (kitDir </> $(mkRelFile "Malformed dummy kit pack v1.0/santos_1.png")) `shouldReturn` True
          it "won't install a damaged archive file" $
            withTmpUserDir $ \dir ->
              do (damagedZipFile >>= runInstallMod . installKitPack dir) `shouldThrow` anyKitPackException
                 (damagedRarFile >>= runInstallMod . installKitPack dir) `shouldThrow` anyKitPackException
          it "fails if the archive format is unsupported" $
            withTmpUserDir $ \dir ->
              (unsupportedFile >>= runInstallMod . installKitPack dir) `shouldThrow` anyKitPackException
          it "fails if the specified archive doesn't exist" $
            withTmpUserDir $ \dir ->
              runInstallMod (installKitPack dir (ArchiveFilePath $(mkAbsFile "/this/doesn't/exist.zip"))) `shouldThrow` anyIOException
          it "fails if the user directory doesn't exist" $
            withTmpUserDir $ \dir ->
              let missingUserDir = UserDirPath $ _userDirPath dir </> $(mkRelDir "missing")
              in
                (dummyPackV10Zip >>= runInstallMod . installKitPack missingUserDir) `shouldThrow` anyKitPackException
