{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.UtilSpec
       ( spec
       ) where

import Path ((</>), mkRelFile)
import Path.IO (doesFileExist, withSystemTempDir)
import Test.Hspec

import Game.FMAssistant.Util

spec :: Spec
spec =
  do describe "basename" $
       it "should return the filename stripped of extension" $
         basename $(mkRelFile "xyz/abc.def") `shouldBe` "abc"
     describe "touchFile" $
       do it "should create a new file" $
            withSystemTempDir "UtilSpec" $ \dir ->
              let testFile = dir </> $(mkRelFile "testTouchFile")
              in do touchFile testFile
                    doesFileExist testFile `shouldReturn` True
          it "should be idempotent" $
            withSystemTempDir "UtilSpec" $ \dir ->
              let testFile = dir </> $(mkRelFile "testTouchFile")
              in do touchFile testFile
                    touchFile testFile
                    doesFileExist testFile `shouldReturn` True
