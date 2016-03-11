{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.FM16Spec
       ( spec
       ) where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Game.FMAssistant.Types (UserDirFilePath(..), Version(..))
import Game.FMAssistant.FM16

spec :: Spec
spec =
  do describe "version" $
       it "is the expected value" $
         version `shouldBe` Version "Football Manager 2016"
     describe "defaultUserDir" $
       it "is the expected value" $
         do homeDir <- getHomeDirectory
            defaultUserDir `shouldReturn` UserDirFilePath (homeDir </> "Documents/Sports Interactive/Football Manager 2016")
