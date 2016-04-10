{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.RepackSpec
       ( spec
       ) where

import Path (mkAbsFile)
import Test.Hspec

import Game.FMAssistant.Repack

spec :: Spec
spec =
  do describe "archiveName" $
       it "returns the basename of an 'ArchiveFilePath'" $
         do archiveName (ArchiveFilePath $(mkAbsFile "/foo/bar/baz.rar" )) `shouldBe` "baz"
            archiveName (ArchiveFilePath $(mkAbsFile "/foo/bar/foo bar.zip" )) `shouldBe` "foo bar"
            archiveName (ArchiveFilePath $(mkAbsFile "/foo/bar/baz qux" )) `shouldBe` "baz qux"
