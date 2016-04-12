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
            do it "returns the proper version directory" $
                 do versionDir FM16 `shouldBe` $(mkRelDir "Football Manager 2016")
     describe "defaultUserDir" $
       do context "FM16" $
            do it "returns the expected value" $
                 do homeDir <- getHomeDir
                    defaultUserDir FM16 `shouldReturn` UserDirPath (homeDir </> $(mkRelDir "Documents/Sports Interactive/Football Manager 2016"))
     describe "defaultAppDir" $
       do context "FM16" $
            do it "returns the expected value" $
                 do homeDir <- getHomeDir
                    defaultAppDir FM16 `shouldReturn` AppDirPath (homeDir </> $(mkRelDir "Library/Application Support/Steam/SteamApps/common/Football Manager 2016"))
     describe "defaultBackupDir" $
       do it "returns the expected value" $
            do homeDir <- getHomeDir
               defaultBackupDir `shouldReturn` BackupDirPath (homeDir </> $(mkRelDir "Library/Application Support/FMAssistant"))
