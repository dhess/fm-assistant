{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.Mod.ModInstallSpec
       ( spec
       ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Path ((</>), parseAbsFile, parseRelDir, parseRelFile)
import Path.IO (doesFileExist, removeDir)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Repack.Unpack
import Game.FMAssistant.Util (createSystemTempDir)
import Game.FMAssistant.Types (ArchiveFilePath(..), UserDirPath(..))

getArchiveFilePath :: FilePath -> IO ArchiveFilePath
getArchiveFilePath fp =
  do fn <- getDataFileName fp
     absfn <- parseAbsFile fn
     return $ ArchiveFilePath absfn

dummyPackV10Zip :: IO ArchiveFilePath
dummyPackV10Zip = getArchiveFilePath "data/test/Dummy kit pack v1.0.zip"

dummyPackV11Zip :: IO ArchiveFilePath
dummyPackV11Zip = getArchiveFilePath "data/test/Dummy kit pack v1.1.zip"

spec :: Spec
spec =
  do describe "Installing a mod" $
       it "should work" $
         do pendingWith "tests need to be ported from old system"

-- These tests were written for the old 'installMod' scheme, need to
-- be rewritten for the new pack-based scheme.

-- spec :: Spec
-- spec =
--   do describe "installMod" $
--        do it "should install a mod" $
--             do v10 <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     unpack v10 (UnpackDirPath tmpSrc)
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ installMod (UnpackDirPath $ tmpSrc </> modDir) (tmpDst </> modDir)
--                     config_xml <- parseRelFile "Dummy kit pack/config.xml"
--                     liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
--                     flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
--                     liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
--                     santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
--                     liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` True
--           it "should not install the mod when it already exists" $
--             do v10 <- dummyPackV10Zip
--                v11 <- dummyPackV11Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc10) <- createSystemTempDir "InstallSpec_tmpSrc10"
--                     unpack v10 (UnpackDirPath tmpSrc10)
--                     (_, tmpSrc11) <- createSystemTempDir "InstallSpec_tmpSrc11"
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ installMod (UnpackDirPath $ tmpSrc10 </> modDir) (tmpDst </> modDir)
--                     unpack v11 (UnpackDirPath tmpSrc11)
--                     liftIO $ installMod (UnpackDirPath $ tmpSrc11 </> modDir) (tmpDst </> modDir) `shouldThrow` anyIOException
--           it "should fail when the source directory doesn't exist" $
--             do _ <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ installMod (UnpackDirPath $ tmpSrc </> modDir) (tmpDst </> modDir) `shouldThrow` anyIOException
--           it "should fail when some part of the destination path doesn't exist" $
--             do v10 <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     unpack v10 (UnpackDirPath tmpSrc)
--                     removeDir tmpDst
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ installMod (UnpackDirPath $ tmpSrc </> modDir) (tmpDst </> modDir) `shouldThrow` anyIOException
--      describe "replaceMod" $
--        do it "should install a mod" $
--             do v10 <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     unpack v10 (UnpackDirPath tmpSrc)
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ replaceMod (UnpackDirPath $ tmpSrc </> modDir) (tmpDst </> modDir)
--                     config_xml <- parseRelFile "Dummy kit pack/config.xml"
--                     liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
--                     flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
--                     liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
--                     santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
--                     liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` True
--           it "should overwrite an existing installation of the same mod" $
--             do v10 <- dummyPackV10Zip
--                v11 <- dummyPackV11Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc10) <- createSystemTempDir "InstallSpec_tmpSrc10"
--                     unpack v10 (UnpackDirPath tmpSrc10)
--                     (_, tmpSrc11) <- createSystemTempDir "InstallSpec_tmpSrc11"
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ replaceMod (UnpackDirPath $ tmpSrc10 </> modDir) (tmpDst </> modDir)
--                     unpack v11 (UnpackDirPath tmpSrc11)
--                     liftIO $ replaceMod (UnpackDirPath $ tmpSrc11 </> modDir) (tmpDst </> modDir)
--                     config_xml <- parseRelFile "Dummy kit pack/config.xml"
--                     liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
--                     flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
--                     liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
--                     vasco_1_png  <- parseRelFile "Dummy kit pack/vasco_1.png"
--                     liftIO $ doesFileExist (tmpDst </> vasco_1_png) `shouldReturn` True
--                     santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
--                     liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` False
--           it "should fail when the source directory doesn't exist" $
--             do _ <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ replaceMod (UnpackDirPath $ tmpSrc </> modDir) (tmpDst </> modDir) `shouldThrow` anyIOException
--           it "should fail when some part of the destination path doesn't exist" $
--             do v10 <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     unpack v10 (UnpackDirPath tmpSrc)
--                     removeDir tmpDst
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ replaceMod (UnpackDirPath $ tmpSrc </> modDir) (tmpDst </> modDir) `shouldThrow` anyIOException
--      describe "install with installMod" $
--        do it "should install a mod" $
--             do v10 <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     unpack v10 (UnpackDirPath tmpSrc)
--                     modDir <- parseRelDir "Dummy kit pack"
--                     runReaderT (install (UnpackDirPath $ tmpSrc </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) installMod)
--                     config_xml <- parseRelFile "Dummy kit pack/config.xml"
--                     liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
--                     flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
--                     liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
--                     santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
--                     liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` True
--           it "should not install the mod when it already exists" $
--             do v10 <- dummyPackV10Zip
--                v11 <- dummyPackV11Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc10) <- createSystemTempDir "InstallSpec_tmpSrc10"
--                     unpack v10 (UnpackDirPath tmpSrc10)
--                     (_, tmpSrc11) <- createSystemTempDir "InstallSpec_tmpSrc11"
--                     modDir <- parseRelDir "Dummy kit pack"
--                     runReaderT (install (UnpackDirPath $ tmpSrc10 </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) installMod)
--                     unpack v11 (UnpackDirPath tmpSrc11)
--                     liftIO $ runReaderT (install (UnpackDirPath $ tmpSrc11 </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) installMod) `shouldThrow` anyIOException
--           it "should fail when the source directory doesn't exist" $
--             do _ <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ runReaderT (install (UnpackDirPath $ tmpSrc </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) installMod) `shouldThrow` anyIOException
--           it "should fail when some part of the destination path doesn't exist" $
--             do v10 <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     unpack v10 (UnpackDirPath tmpSrc)
--                     removeDir tmpDst
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ runReaderT (install (UnpackDirPath $ tmpSrc </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) installMod) `shouldThrow` anyIOException
--      describe "install with replaceMod" $
--        do it "should install a mod" $
--             do v10 <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     unpack v10 (UnpackDirPath tmpSrc)
--                     modDir <- parseRelDir "Dummy kit pack"
--                     runReaderT (install (UnpackDirPath $ tmpSrc </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) replaceMod)
--                     config_xml <- parseRelFile "Dummy kit pack/config.xml"
--                     liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
--                     flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
--                     liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
--                     santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
--                     liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` True
--           it "should overwrite an existing installation of the same mod" $
--             do v10 <- dummyPackV10Zip
--                v11 <- dummyPackV11Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc10) <- createSystemTempDir "InstallSpec_tmpSrc10"
--                     unpack v10 (UnpackDirPath tmpSrc10)
--                     (_, tmpSrc11) <- createSystemTempDir "InstallSpec_tmpSrc11"
--                     modDir <- parseRelDir "Dummy kit pack"
--                     runReaderT (install (UnpackDirPath $ tmpSrc10 </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) replaceMod)
--                     unpack v11 (UnpackDirPath tmpSrc11)
--                     runReaderT (install (UnpackDirPath $ tmpSrc11 </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) replaceMod)
--                     config_xml <- parseRelFile "Dummy kit pack/config.xml"
--                     liftIO $ doesFileExist (tmpDst </> config_xml) `shouldReturn` True
--                     flamengo_1_png  <- parseRelFile "Dummy kit pack/flamengo_1.png"
--                     liftIO $ doesFileExist (tmpDst </> flamengo_1_png) `shouldReturn` True
--                     vasco_1_png  <- parseRelFile "Dummy kit pack/vasco_1.png"
--                     liftIO $ doesFileExist (tmpDst </> vasco_1_png) `shouldReturn` True
--                     santos_1_png  <- parseRelFile "Dummy kit pack/santos_1.png"
--                     liftIO $ doesFileExist (tmpDst </> santos_1_png) `shouldReturn` False
--           it "should fail when the source directory doesn't exist" $
--             do _ <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ runReaderT (install (UnpackDirPath $ tmpSrc </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) replaceMod) `shouldThrow` anyIOException
--           it "should fail when some part of the destination path doesn't exist" $
--             do v10 <- dummyPackV10Zip
--                runResourceT $
--                  do (_, tmpDst) <- createSystemTempDir "InstallSpec_tmpDst"
--                     (_, tmpSrc) <- createSystemTempDir "InstallSpec_tmpSrc"
--                     unpack v10 (UnpackDirPath tmpSrc)
--                     removeDir tmpDst
--                     modDir <- parseRelDir "Dummy kit pack"
--                     liftIO $ runReaderT (install (UnpackDirPath $ tmpSrc </> modDir) modDir) (InstallConfig (UserDirPath tmpDst) replaceMod) `shouldThrow` anyIOException
