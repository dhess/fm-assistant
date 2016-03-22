{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.InstallSpec
       ( spec
       ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Path ((</>), Path, Abs, Rel, Dir, File, dirname, parent, parseAbsFile, parseRelDir, parseRelFile, toFilePath)
import Path.IO (doesDirExist, doesFileExist, removeDirRecur, renameDir)
import qualified Path.IO as Path (createTempDir)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Install
import Game.FMAssistant.Unpack
import Game.FMAssistant.Util (createSystemTempDir)
import Game.FMAssistant.Types (ArchiveFilePath(..))

dummyPackV10Zip :: IO ArchiveFilePath
dummyPackV10Zip = ArchiveFilePath <$> getDataFileName "data/test/Dummy kit pack v1.0.zip"

dummyPackV11Zip :: IO ArchiveFilePath
dummyPackV11Zip = ArchiveFilePath <$> getDataFileName "data/test/Dummy kit pack v1.1.zip"

spec :: Spec
spec =
  do describe "installMod" $
       do it "should install a mod" $
            do v10 <- dummyPackV10Zip
               runResourceT $
                 do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
                    (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
                    unpack v10 (toFilePath tmpSrc)
                    modDir <- parseRelDir "Dummy kit pack"
                    installMod (tmpSrc </> modDir) (tmpDst </> modDir)
                    config_xml <- parseRelFile "Dummy kit pack/config.xml"
                    liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
                    flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
                    liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
                    santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
                    liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` True
          it "should not install the mod when it already exists" $
            do v10 <- dummyPackV10Zip
               v11 <- dummyPackV11Zip
               runResourceT $
                 do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
                    (_, tmpSrc10) <- createSystemTempDir "InstallSpec_tmpSrc10"
                    unpack v10 (toFilePath tmpSrc10)
                    (_, tmpSrc11) <- createSystemTempDir "InstallSpec_tmpSrc11"
                    modDir <- parseRelDir "Dummy kit pack"
                    installMod (tmpSrc10 </> modDir) (tmpDst </> modDir)
                    unpack v11 (toFilePath tmpSrc11)
                    liftIO $ installMod (tmpSrc11 </> modDir) (tmpDst </> modDir) `shouldThrow` anyIOException
     describe "replaceMod" $
       do it "should install a mod" $
            do v10 <- dummyPackV10Zip
               runResourceT $
                 do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
                    (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
                    unpack v10 (toFilePath tmpSrc)
                    modDir <- parseRelDir "Dummy kit pack"
                    replaceMod (tmpSrc </> modDir) (tmpDst </> modDir)
                    config_xml <- parseRelFile "Dummy kit pack/config.xml"
                    liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
                    flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
                    liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
                    santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
                    liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` True
          it "should overwrite an existing installation of the same mod" $
            do v10 <- dummyPackV10Zip
               v11 <- dummyPackV11Zip
               runResourceT $
                 do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
                    (_, tmpSrc10) <- createSystemTempDir "InstallSpec_tmpSrc10"
                    unpack v10 (toFilePath tmpSrc10)
                    (_, tmpSrc11) <- createSystemTempDir "InstallSpec_tmpSrc11"
                    modDir <- parseRelDir "Dummy kit pack"
                    replaceMod (tmpSrc10 </> modDir) (tmpDst </> modDir)
                    unpack v11 (toFilePath tmpSrc11)
                    replaceMod (tmpSrc11 </> modDir) (tmpDst </> modDir)
                    config_xml <- parseRelFile "Dummy kit pack/config.xml"
                    liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
                    flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
                    liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
                    vasco_1_png  <- parseRelFile "Dummy kit pack/vasco_1.png"
                    liftIO $ doesFileExist (tmpDst </> vasco_1_png) `shouldReturn` True
                    santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
                    liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` False
     describe "runInstallModT" $
       do it "should install a mod" $
            do v10 <- dummyPackV10Zip
               runResourceT $
                 do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
                    (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
                    unpack v10 (toFilePath tmpSrc)
                    modDir <- parseRelDir "Dummy kit pack"
                    runInstallModT $ install (tmpSrc </> modDir) (tmpDst </> modDir)
                    config_xml <- parseRelFile "Dummy kit pack/config.xml"
                    liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
                    flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
                    liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
                    santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
                    liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` True
          it "should not install the mod when it already exists" $
            do v10 <- dummyPackV10Zip
               v11 <- dummyPackV11Zip
               runResourceT $
                 do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
                    (_, tmpSrc10) <- createSystemTempDir "InstallSpec_tmpSrc10"
                    unpack v10 (toFilePath tmpSrc10)
                    (_, tmpSrc11) <- createSystemTempDir "InstallSpec_tmpSrc11"
                    modDir <- parseRelDir "Dummy kit pack"
                    runInstallModT $ install (tmpSrc10 </> modDir) (tmpDst </> modDir)
                    unpack v11 (toFilePath tmpSrc11)
                    liftIO $ runInstallModT (install (tmpSrc11 </> modDir) (tmpDst </> modDir)) `shouldThrow` anyIOException
     describe "runReplaceModT" $
       do it "should install a mod" $
            do v10 <- dummyPackV10Zip
               runResourceT $
                 do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
                    (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
                    unpack v10 (toFilePath tmpSrc)
                    modDir <- parseRelDir "Dummy kit pack"
                    runReplaceModT $ install (tmpSrc </> modDir) (tmpDst </> modDir)
                    config_xml <- parseRelFile "Dummy kit pack/config.xml"
                    liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
                    flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
                    liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
                    santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
                    liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` True
          it "should overwrite an existing installation of the same mod" $
            do v10 <- dummyPackV10Zip
               v11 <- dummyPackV11Zip
               runResourceT $
                 do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
                    (_, tmpSrc10) <- createSystemTempDir "InstallSpec_tmpSrc10"
                    unpack v10 (toFilePath tmpSrc10)
                    (_, tmpSrc11) <- createSystemTempDir "InstallSpec_tmpSrc11"
                    modDir <- parseRelDir "Dummy kit pack"
                    runReplaceModT $ install (tmpSrc10 </> modDir) (tmpDst </> modDir)
                    unpack v11 (toFilePath tmpSrc11)
                    runReplaceModT $ install (tmpSrc11 </> modDir) (tmpDst </> modDir)
                    config_xml <- parseRelFile "Dummy kit pack/config.xml"
                    liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
                    flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
                    liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
                    vasco_1_png  <- parseRelFile "Dummy kit pack/vasco_1.png"
                    liftIO $ doesFileExist (tmpDst </> vasco_1_png) `shouldReturn` True
                    santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
                    liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` False
