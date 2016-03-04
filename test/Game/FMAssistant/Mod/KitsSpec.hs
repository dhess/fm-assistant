{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.Mod.KitsSpec
       ( spec
       ) where

import Prelude hiding (FilePath)
import Test.Hspec

import Game.FMAssistant.Types (UserDirFilePath(..))
import Game.FMAssistant.Mod.Kits

spec :: Spec
spec =
  describe "kitPath" $
    it "constructs 'KitPath's from 'UserDirFilePath's" $
      do filePath (kitPath (UserDirFilePath "/foo/bar")) `shouldBe` "/foo/bar/graphics/kits"
         filePath (kitPath (UserDirFilePath "baz/qux/")) `shouldBe` "baz/qux/graphics/kits"
