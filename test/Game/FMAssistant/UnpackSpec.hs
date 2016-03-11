{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Game.FMAssistant.UnpackSpec
       ( spec
       ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (runManaged)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Unpack
import Game.FMAssistant.Util (mktempdir)

unsupportedFile :: IO ArchiveFilePath
unsupportedFile = ArchiveFilePath <$> getDataFileName "data/test/test.tar"

zipFile :: IO ArchiveFilePath
zipFile = ArchiveFilePath <$> getDataFileName "data/test/test.zip"

damagedZipFile :: IO ArchiveFilePath
damagedZipFile = ArchiveFilePath <$> getDataFileName "data/test/damaged-test.zip"

rarFile :: IO ArchiveFilePath
rarFile = ArchiveFilePath <$> getDataFileName "data/test/test.rar"

damagedRarFile :: IO ArchiveFilePath
damagedRarFile = ArchiveFilePath <$> getDataFileName "data/test/damaged-test.rar"

anyUnpackException :: Selector UnpackException
anyUnpackException = const True

spec :: Spec
spec =
  do describe "unpackZip" $
       do it "should unpack a zip file to a temporary directory" $
            runManaged $
              do zf <- liftIO zipFile
                 tmpDir <- mktempdir "UnpackSpec"
                 unpackZip zf tmpDir
                 sysTmpDir <- liftIO getTemporaryDirectory
                 liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
          it "should unpack a.txt from the test file" $
            runManaged $
              do zf <- liftIO zipFile
                 tmpDir <- mktempdir "UnpackSpec"
                 unpackZip zf tmpDir
                 liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
          it "should unpack b.txt from the test file" $
            runManaged $
              do zf <- liftIO zipFile
                 tmpDir <- mktempdir "UnpackSpec"
                 unpackZip zf tmpDir
                 liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
          it "should unpack foo/c.txt from the test file" $
            runManaged $
              do zf <- liftIO zipFile
                 tmpDir <- mktempdir "UnpackSpec"
                 unpackZip zf tmpDir
                 liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          it "should fail when used on a damaged ZIP file" $
            (runManaged $
               do zf <- liftIO damagedZipFile
                  tmpDir <- mktempdir "UnpackSpec"
                  unpackZip zf tmpDir) `shouldThrow` anyUnpackException
          it "should fail when used on a RAR file" $
            (runManaged $
               do rf <- liftIO rarFile
                  tmpDir <- mktempdir "UnpackSpec"
                  unpackZip rf tmpDir) `shouldThrow` anyUnpackException
          it "should fail when the file doesn't exist" $
            (runManaged $ mktempdir "UnpackSpec" >>= unpackZip (ArchiveFilePath "/does/not/exist.zip")) `shouldThrow` anyUnpackException
     describe "unpackRar" $
       do it "should unpack a rar file to a temporary directory" $
            runManaged $
              do rf <- liftIO rarFile
                 tmpDir <- mktempdir "UnpackSpec"
                 unpackRar rf tmpDir
                 sysTmpDir <- liftIO getTemporaryDirectory
                 liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
          it "should unpack a.txt from the test file" $
            runManaged $
              do rf <- liftIO rarFile
                 tmpDir <- mktempdir "UnpackSpec"
                 unpackRar rf tmpDir
                 liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
          it "should unpack b.txt from the test file" $
            runManaged $
              do rf <- liftIO rarFile
                 tmpDir <- mktempdir "UnpackSpec"
                 unpackRar rf tmpDir
                 liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
          it "should unpack foo/c.txt from the test file" $
            runManaged $
              do rf <- liftIO rarFile
                 tmpDir <- mktempdir "UnpackSpec"
                 unpackRar rf tmpDir
                 liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          it "should fail when used on a damaged RAR file" $
            (runManaged $
               do rf <- liftIO damagedRarFile
                  tmpDir <- mktempdir "UnpackSpec"
                  unpackRar rf tmpDir) `shouldThrow` anyUnpackException
          it "should fail when used on a ZIP file" $
            (runManaged $
               do zf <- liftIO zipFile
                  tmpDir <- mktempdir "UnpackSpec"
                  unpackRar zf tmpDir) `shouldThrow` anyUnpackException
          it "should fail when the file doesn't exist" $
            (runManaged $ mktempdir "UnpackSpec" >>= unpackRar (ArchiveFilePath "/does/not/exist.rar")) `shouldThrow` anyUnpackException
     describe "unpack" $
       do context "on RAR files" $
            do it "should unpack the file to a temporary directory" $
                 runManaged $
                   do rf <- liftIO rarFile
                      tmpDir <- unpack rf
                      sysTmpDir <- liftIO getTemporaryDirectory
                      liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
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
                      sysTmpDir <- liftIO getTemporaryDirectory
                      liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
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
              (runManaged $ void $ unpack (ArchiveFilePath "/does/not/exist.zip")) `shouldThrow` anyIOException
     describe "unpackerFor" $
       do context "on files with a \"rar\" extension" $
            do it "should unpack a rar file to a temporary directory" $
                 do rf <- liftIO rarFile
                    runManaged $
                      do Just unp <- unpackerFor rf
                         tmpDir <- mktempdir "UnpackSpec"
                         unp rf tmpDir
                         sysTmpDir <- liftIO getTemporaryDirectory
                         liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
               it "should unpack a.txt from the test file" $
                 do rf <- liftIO rarFile
                    runManaged $
                      do Just unp <- unpackerFor rf
                         tmpDir <- mktempdir "UnpackSpec"
                         unp rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do rf <- liftIO rarFile
                    runManaged $
                      do Just unp <- unpackerFor rf
                         tmpDir <- mktempdir "UnpackSpec"
                         unp rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do rf <- liftIO rarFile
                    runManaged $
                      do Just unp <- unpackerFor rf
                         tmpDir <- mktempdir "UnpackSpec"
                         unp rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          context "on files with a \"zip\" extension" $
            do it "should unpack a zip file to a temporary directory" $
                 do zf <- liftIO zipFile
                    runManaged $
                      do Just unp <- unpackerFor zf
                         tmpDir <- mktempdir "UnpackSpec"
                         unp zf tmpDir
                         sysTmpDir <- liftIO getTemporaryDirectory
                         liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
               it "should unpack a.txt from the test file" $
                 do zf <- liftIO zipFile
                    runManaged $
                      do Just unp <- unpackerFor zf
                         tmpDir <- mktempdir "UnpackSpec"
                         unp zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do zf <- liftIO zipFile
                    runManaged $
                      do Just unp <- unpackerFor zf
                         tmpDir <- mktempdir "UnpackSpec"
                         unp zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do zf <- liftIO zipFile
                    runManaged $
                      do Just unp <- unpackerFor zf
                         tmpDir <- mktempdir "UnpackSpec"
                         unp zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          context "on unsupported archive types" $
            it "should return 'Nothing'" $
              do unsupported <- liftIO unsupportedFile
                 isNothing <$> unpackerFor unsupported `shouldReturn` True
          -- context "on files with no extension" $
          --   it "should return 'Nothing'" $
          --     isNothing <$> unpackerFor (ArchiveFilePath "foo/rar/not-a-zip") `shouldReturn` True
