{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.FM16Spec
       ( spec
       ) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS ((</>), FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString)
import qualified System.Directory as Directory (getHomeDirectory)
import Test.Hspec

import qualified Game.FMAssistant.Mod.Kits as Kits (filePath)
import Game.FMAssistant.Types (UserDirFilePath(..), Version(..))
import Game.FMAssistant.FM16

getHomeDir :: IO FilePath
getHomeDir = Filesystem.decodeString <$> Directory.getHomeDirectory

spec :: Spec
spec =
  do describe "version" $
       it "is the expected value" $
         version `shouldBe` Version "Football Manager 2016"
     describe "defaultUserDir" $
       it "is the expected value" $
         do homeDir <- getHomeDir
            defaultUserDir `shouldReturn` UserDirFilePath (homeDir </> "Documents/Sports Interactive/Football Manager 2016")
     describe "defaultKitDir" $
       it "is the expected value" $
         do homeDir <- getHomeDir
            Kits.filePath <$> defaultKitDir `shouldReturn` (homeDir </> "Documents/Sports Interactive/Football Manager 2016/graphics/kits")
