{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.VersionSpec
       ( spec
       ) where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Game.FMAssistant.Types (UserDirFilePath(..), Version(..))
import Game.FMAssistant.Version

spec :: Spec
spec =
  do context "runFM16" $
       do describe "version" $
            it "is the expected value" $
              runFM16 version `shouldReturn` Version "Football Manager 2016"
          describe "defaultUserDir" $
            it "is the expected value" $
              do homeDir <- getHomeDirectory
                 runFM16 defaultUserDir `shouldReturn` UserDirFilePath (homeDir </> "Documents" </> "Sports Interactive" </> "Football Manager 2016")
