{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.UtilSpec
       ( spec
       ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (runManaged)
import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, getHomeDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Game.FMAssistant.Types (UserDirFilePath(..), Version(..))
import Game.FMAssistant.Util

spec :: Spec
spec =
  do describe "defaultUserDir" $
       it "returns the expected value" $
         do homeDir <- getHomeDirectory
            defaultUserDir (Version "foo bar") `shouldReturn` UserDirFilePath (homeDir </> "Documents/Sports Interactive/foo bar")
     describe "mktempdir" $
       do it "creates a temporary directory in the system temporary directory" $
            do sysTmpDir <- getTemporaryDirectory
               runManaged $
                 do tmpDir <- mktempdir "Foo"
                    liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
                    liftIO $ doesDirectoryExist tmpDir `shouldReturn` True
