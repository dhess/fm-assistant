{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.VersionSpec
       ( spec
       ) where

import Path ((</>), mkRelDir)
import Path.IO (getHomeDir)
import Test.Hspec

import Game.FMAssistant.Types (UserDirPath(..), VersionDirPath(..))
import Game.FMAssistant.Version

spec :: Spec
spec =
  do context "runFM16" $
       do describe "version" $
            it "is the expected value" $
              runFM16 version `shouldReturn` VersionDirPath $(mkRelDir "Football Manager 2016")
          describe "defaultUserDir" $
            it "is the expected value" $
              do homeDir <- getHomeDir
                 runFM16 defaultUserDirPath `shouldReturn` UserDirPath (homeDir </> $(mkRelDir "Documents/Sports Interactive/Football Manager 2016"))
