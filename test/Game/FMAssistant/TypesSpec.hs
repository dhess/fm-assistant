{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.TypesSpec
       ( spec
       ) where

import Path ((</>), mkRelDir)
import Path.IO (getHomeDir)
import Test.Hspec

import Game.FMAssistant.Types

spec :: Spec
spec =
  do describe "versionDir" $
       do context "FM16" $
            it "returns the proper version directory" $
              do versionDir FM16 `shouldBe` $(mkRelDir "Football Manager 2016")
          context "FM17" $
            it "returns the proper version directory" $
              do versionDir FM17 `shouldBe` $(mkRelDir "Football Manager 2017")
     describe "defaultUserDir" $
       do context "FM16" $
            it "returns the expected value" $
              do homeDir <- getHomeDir
                 defaultUserDir FM16 `shouldReturn` UserDirPath (homeDir </> $(mkRelDir "Documents/Sports Interactive/Football Manager 2016"))
          context "FM17" $
            it "returns the expected value" $
              do homeDir <- getHomeDir
                 defaultUserDir FM17 `shouldReturn` UserDirPath (homeDir </> $(mkRelDir "Documents/Sports Interactive/Football Manager 2017"))
     describe "defaultAppDir" $
       do context "FM16" $
            it "returns the expected value" $
              do homeDir <- getHomeDir
                 defaultAppDir FM16 `shouldReturn` AppDirPath (homeDir </> $(mkRelDir "Library/Application Support/Steam/SteamApps/common/Football Manager 2016"))
          context "FM17" $
            it "returns the expected value" $
              do homeDir <- getHomeDir
                 defaultAppDir FM17 `shouldReturn` AppDirPath (homeDir </> $(mkRelDir "Library/Application Support/Steam/SteamApps/common/Football Manager 2017"))
     describe "defaultBackupDir" $
       do context "FM16" $
            it "returns the expected value" $
              do homeDir <- getHomeDir
                 defaultBackupDir FM16 `shouldReturn` BackupDirPath (homeDir </> $(mkRelDir "Library/Application Support/FMAssistant/Football Manager 2016"))
          context "FM17" $
            it "returns the expected value" $
              do homeDir <- getHomeDir
                 defaultBackupDir FM17 `shouldReturn` BackupDirPath (homeDir </> $(mkRelDir "Library/Application Support/FMAssistant/Football Manager 2017"))
