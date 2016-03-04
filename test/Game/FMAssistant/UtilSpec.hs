{-# LANGUAGE OverloadedStrings #-}

module Game.FMAssistant.UtilSpec
       ( spec
       ) where

import Prelude hiding (FilePath)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (runManaged)
import Filesystem.Path.CurrentOS ((</>), FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (commonPrefix, decodeString, encodeString)
import qualified System.Directory as Directory (doesDirectoryExist, getHomeDirectory, getTemporaryDirectory)
import Test.Hspec

import Game.FMAssistant.Types (UserDirFilePath(..), Version(..))
import Game.FMAssistant.Util

getHomeDir :: IO FilePath
getHomeDir = Filesystem.decodeString <$> Directory.getHomeDirectory

spec :: Spec
spec =
  do describe "getHomeDirectory" $
       it "returns the current user's home directory" $
         do homeDir <- getHomeDir
            getHomeDirectory `shouldReturn` homeDir
     describe "defaultUserDir" $
       it "returns the expected value" $
         do homeDir <- getHomeDir
            defaultUserDir (Version "foo bar") `shouldReturn` UserDirFilePath (homeDir </> "Documents/Sports Interactive/foo bar")
     describe "mktempdir" $
       do it "creates a temporary directory in the system temporary directory" $
            do sysTmpDir <- Filesystem.decodeString <$> Directory.getTemporaryDirectory
               runManaged $
                 do tmpDir <- mktempdir "Foo"
                    liftIO $ Filesystem.commonPrefix [sysTmpDir, tmpDir] `shouldBe` sysTmpDir
                    liftIO $ Directory.doesDirectoryExist (Filesystem.encodeString tmpDir) `shouldReturn` True
