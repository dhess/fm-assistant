{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.UtilSpec
       ( spec
       ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, getHomeDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Game.FMAssistant.Util

spec :: Spec
spec =
  do describe "defaultUserDir" $
       it "returns the expected value" $
         do homeDir <- getHomeDirectory
            defaultSteamDir `shouldReturn` (homeDir </> "Documents" </> "Sports Interactive")
     describe "createSystemTempDirectory" $
       do it "creates a temporary directory in the system temporary directory" $
            do sysTmpDir <- getTemporaryDirectory
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDirectory "Foo"
                    liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
                    liftIO $ doesDirectoryExist tmpDir `shouldReturn` True
          it "removes the temporary directory when finished" $
            do tmpDir <- runResourceT $
                 do (_, td) <- createSystemTempDirectory "Foo"
                    return td
               doesDirectoryExist tmpDir `shouldReturn` False
