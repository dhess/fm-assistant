{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.TypesSpec
       ( spec
       ) where

import Path (mkAbsFile, mkRelDir)
import Test.Hspec

import Game.FMAssistant.Types

spec :: Spec
spec =
  do describe "versionDir" $
       do context "FM16"$
            do it "returns the proper version directory" $
                 do versionDir FM16 `shouldBe` $(mkRelDir "Football Manager 2016")
