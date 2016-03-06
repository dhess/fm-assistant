{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Game.FMAssistant.UnpackSpec
       ( spec
       ) where

import Prelude hiding (FilePath)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (runManaged)
import Data.Maybe (isNothing)
import Filesystem.Path.CurrentOS ((</>), FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (commonPrefix, decodeString, encodeString)
import qualified System.Directory as Directory (doesFileExist, getTemporaryDirectory)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Unpack

systemTmpDir :: IO FilePath
systemTmpDir = Filesystem.decodeString <$> Directory.getTemporaryDirectory

doesFileExist :: FilePath -> IO Bool
doesFileExist fp = Directory.doesFileExist (Filesystem.encodeString fp)

zipFile :: IO ArchiveFilePath
zipFile = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/test.zip"

rarFile :: IO ArchiveFilePath
rarFile = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/test.rar"

spec :: Spec
spec =
  do describe "unpackZip" $
       do it "should unpack a zip file to a temporary directory" $
            runManaged $
              do zf <- liftIO zipFile
                 tmpDir <- unpackZip zf
                 sysTmpDir <- liftIO systemTmpDir
                 liftIO $ Filesystem.commonPrefix [sysTmpDir, tmpDir] `shouldBe` sysTmpDir
          it "should unpack a.txt from the test file" $
            runManaged $
              do zf <- liftIO zipFile
                 tmpDir <- unpackZip zf
                 liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
          it "should unpack b.txt from the test file" $
            runManaged $
              do zf <- liftIO zipFile
                 tmpDir <- unpackZip zf
                 liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
          it "should unpack foo/c.txt from the test file" $
            runManaged $
              do zf <- liftIO zipFile
                 tmpDir <- unpackZip zf
                 liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
     describe "unpackRar" $
       do it "should unpack a rar file to a temporary directory" $
            runManaged $
              do rf <- liftIO rarFile
                 tmpDir <- unpackRar rf
                 sysTmpDir <- liftIO systemTmpDir
                 liftIO $ Filesystem.commonPrefix [sysTmpDir, tmpDir] `shouldBe` sysTmpDir
          it "should unpack a.txt from the test file" $
            runManaged $
              do rf <- liftIO rarFile
                 tmpDir <- unpackRar rf
                 liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
          it "should unpack b.txt from the test file" $
            runManaged $
              do rf <- liftIO rarFile
                 tmpDir <- unpackRar rf
                 liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
          it "should unpack foo/c.txt from the test file" $
            runManaged $
              do rf <- liftIO rarFile
                 tmpDir <- unpackRar rf
                 liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
     describe "unpackerFor" $
       do context "on files with a \"rar\" extension" $
            do it "should unpack a rar file to a temporary directory" $
                 do rf <- liftIO rarFile
                    let Just unp = unpackerFor rf
                    runManaged $
                      do tmpDir <- unp rf
                         sysTmpDir <- liftIO systemTmpDir
                         liftIO $ Filesystem.commonPrefix [sysTmpDir, tmpDir] `shouldBe` sysTmpDir
               it "should unpack a.txt from the test file" $
                 do rf <- liftIO rarFile
                    let Just unp = unpackerFor rf
                    runManaged $
                      do tmpDir <- unp rf
                         liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do rf <- liftIO rarFile
                    let Just unp = unpackerFor rf
                    runManaged $
                      do tmpDir <- unp rf
                         liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do rf <- liftIO rarFile
                    let Just unp = unpackerFor rf
                    runManaged $
                      do tmpDir <- unp rf
                         liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          context "on files with a \"zip\" extension" $
            do it "should unpack a zip file to a temporary directory" $
                 do zf <- liftIO zipFile
                    let Just unp = unpackerFor zf
                    runManaged $
                      do tmpDir <- unp zf
                         sysTmpDir <- liftIO systemTmpDir
                         liftIO $ Filesystem.commonPrefix [sysTmpDir, tmpDir] `shouldBe` sysTmpDir
               it "should unpack a.txt from the test file" $
                 do zf <- liftIO zipFile
                    let Just unp = unpackerFor zf
                    runManaged $
                      do tmpDir <- unp zf
                         liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do zf <- liftIO zipFile
                    let Just unp = unpackerFor zf
                    runManaged $
                      do tmpDir <- unp zf
                         liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do zf <- liftIO zipFile
                    let Just unp = unpackerFor zf
                    runManaged $
                      do tmpDir <- unp zf
                         liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          context "on files without a supported extension" $
            it "should return 'Nothing'" $
              isNothing (unpackerFor (ArchiveFilePath "foo/rar/not-a-rar.xyz")) `shouldBe` True
          context "on files with no extension" $
            it "should return 'Nothing'" $
              isNothing (unpackerFor (ArchiveFilePath "foo/rar/not-a-zip")) `shouldBe` True
