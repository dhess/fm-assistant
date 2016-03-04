{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.TypesSpec
       ( spec
       ) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)
import Test.Hspec

import Game.FMAssistant.Types

spec :: Spec
spec =
  do describe "versionToFilePath" $
       it "converts a 'Version' to a 'FilePath'" $
         let version = Version "Foo Bar"
         in
           versionToFilePath version `shouldBe` ("Foo Bar" :: FilePath)
