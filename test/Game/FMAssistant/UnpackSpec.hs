{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Game.FMAssistant.UnpackSpec
       ( spec
       ) where

import Prelude hiding (FilePath)
import Control.Monad (void)
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

damagedZipFile :: IO ArchiveFilePath
damagedZipFile = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/damaged-test.zip"

rarFile :: IO ArchiveFilePath
rarFile = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/test.rar"

damagedRarFile :: IO ArchiveFilePath
damagedRarFile = ArchiveFilePath <$> Filesystem.decodeString <$> getDataFileName "data/test/damaged-test.rar"

anyUnpackException :: Selector UnpackException
anyUnpackException = const True

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
          it "should fail when used on a damaged ZIP file" $
            (runManaged $
               do zf <- liftIO damagedZipFile
                  void $ unpackZip zf) `shouldThrow` anyUnpackException
          it "should fail when used on a RAR file" $
            (runManaged $
               do rf <- liftIO rarFile
                  void $ unpackZip rf) `shouldThrow` anyUnpackException
          it "should fail when the file doesn't exist" $
            (runManaged $ void $ unpackZip (ArchiveFilePath "/does/not/exist.zip")) `shouldThrow` anyUnpackException
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
          it "should fail when used on a damaged RAR file" $
            (runManaged $
               do rf <- liftIO damagedRarFile
                  void $ unpackRar rf) `shouldThrow` anyUnpackException
          it "should fail when used on a ZIP file" $
            (runManaged $
               do zf <- liftIO zipFile
                  void $ unpackRar zf) `shouldThrow` anyUnpackException
          it "should fail when the file doesn't exist" $
            (runManaged $ void $ unpackRar (ArchiveFilePath "/does/not/exist.rar")) `shouldThrow` anyUnpackException
     describe "unpack" $
       do context "on RAR files" $
            do it "should unpack the file to a temporary directory" $
                 runManaged $
                   do rf <- liftIO rarFile
                      tmpDir <- unpack rf
                      sysTmpDir <- liftIO systemTmpDir
                      liftIO $ Filesystem.commonPrefix [sysTmpDir, tmpDir] `shouldBe` sysTmpDir
               it "should unpack a.txt from the test file" $
                 runManaged $
                   do rf <- liftIO rarFile
                      tmpDir <- unpack rf
                      liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 runManaged $
                   do rf <- liftIO rarFile
                      tmpDir <- unpack rf
                      liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 runManaged $
                   do rf <- liftIO rarFile
                      tmpDir <- unpack rf
                      liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
               it "should fail when used on a damaged file" $
                 (runManaged $
                    do rf <- liftIO damagedRarFile
                       void$ unpack rf) `shouldThrow` anyUnpackException
          context "on ZIP files" $
            do it "should unpack the file to a temporary directory" $
                 runManaged $
                   do zf <- liftIO zipFile
                      tmpDir <- unpack zf
                      sysTmpDir <- liftIO systemTmpDir
                      liftIO $ Filesystem.commonPrefix [sysTmpDir, tmpDir] `shouldBe` sysTmpDir
               it "should unpack a.txt from the test file" $
                 runManaged $
                   do zf <- liftIO zipFile
                      tmpDir <- unpack zf
                      liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 runManaged $
                   do zf <- liftIO zipFile
                      tmpDir <- unpack zf
                      liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 runManaged $
                   do zf <- liftIO zipFile
                      tmpDir <- unpack zf
                      liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
               it "should fail when used on a damaged file" $
                 (runManaged $
                    do zf <- liftIO damagedZipFile
                       void$ unpack zf) `shouldThrow` anyUnpackException
          context "on non-existent files" $
            it "should fail" $
              (runManaged $ void $ unpack (ArchiveFilePath "/does/not/exist.zip")) `shouldThrow` anyUnpackException
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
