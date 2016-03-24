{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.UtilSpec
       ( spec
       ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Path ((</>), isParentOf, mkRelDir)
import Path.IO (doesDirExist, getHomeDir, getTempDir)
import Test.Hspec

import Game.FMAssistant.Util

spec :: Spec
spec =
  do describe "defaultSteamDir" $
       it "returns the expected value" $
         do homeDir <- getHomeDir
            defaultSteamDir `shouldReturn` (homeDir </> $(mkRelDir "Documents/Sports Interactive"))
     -- describe "createSystemTempDir" $
     --   do it "creates a temporary directory in the system temporary directory" $
     --        runResourceT $
     --          do (_, tmpDir) <- createSystemTempDir "Foo"
     --             sysTmpDir <- getTempDir
     --             liftIO $ isParentOf sysTmpDir tmpDir `shouldBe` True
     --             liftIO $ doesDirExist tmpDir `shouldReturn` True
     --      it "removes the temporary directory when finished" $
     --        do tmpDir <- runResourceT $
     --             do (_, td) <- createSystemTempDir "Foo"
     --                return td
     --           doesDirExist tmpDir `shouldReturn` False
