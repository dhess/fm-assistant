{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Game.FMAssistant.UnpackSpec
       ( spec
       ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Unpack
import Game.FMAssistant.Util (createSystemTempDirectory)

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
       do it "should unpack a.txt from the test file" $
            do zf <- zipFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                    unpackZip zf tmpDir
                    liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
          it "should unpack b.txt from the test file" $
            do zf <- zipFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                    unpackZip zf tmpDir
                    liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
          it "should unpack c.txt from the test file" $
            do zf <- zipFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                    unpackZip zf tmpDir
                    liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          it "should fail when used on a damaged ZIP file" $
            (runResourceT $
               do zf <- liftIO damagedZipFile
                  (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                  unpackZip zf tmpDir) `shouldThrow` anyUnpackException
          it "should fail when used on a RAR file" $
            (runResourceT $
               do rf <- liftIO rarFile
                  (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                  unpackZip rf tmpDir) `shouldThrow` anyUnpackException
          it "should fail when the file doesn't exist" $
            (runResourceT $ snd <$> createSystemTempDirectory "UnpackSpec" >>= unpackZip (ArchiveFilePath "/does/not/exist.zip")) `shouldThrow` anyUnpackException
     describe "unpackRar" $
       do it "should unpack a.txt from the test file" $
            do rf <- rarFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                    unpackRar rf tmpDir
                    liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
          it "should unpack b.txt from the test file" $
            do rf <- rarFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                    unpackRar rf tmpDir
                    liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
          it "should unpack c.txt from the test file" $
            do rf <- rarFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                    unpackRar rf tmpDir
                    liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          it "should fail when used on a damaged RAR file" $
            (runResourceT $
               do rf <- liftIO damagedRarFile
                  (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                  unpackRar rf tmpDir) `shouldThrow` anyUnpackException
          it "should fail when used on a ZIP file" $
            (runResourceT $
               do zf <- liftIO zipFile
                  (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                  unpackRar zf tmpDir) `shouldThrow` anyUnpackException
          it "should fail when the file doesn't exist" $
            (runResourceT $ snd <$> createSystemTempDirectory "UnpackSpec" >>= unpackRar (ArchiveFilePath "/does/not/exist.rar")) `shouldThrow` anyUnpackException
     describe "unpack" $
       do context "on RAR files" $
            do it "should unpack a.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unpack rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unpack rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unpack rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
               it "should fail when used on a damaged file" $
                 (runResourceT $
                    do rf <- liftIO damagedRarFile
                       (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                       unpack rf tmpDir) `shouldThrow` anyUnpackException
          context "on ZIP files" $
            do it "should unpack a.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unpack zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unpack zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unpack zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
               it "should fail when used on a damaged file" $
                 (runResourceT $
                    do zf <- liftIO damagedZipFile
                       (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                       unpack zf tmpDir) `shouldThrow` anyUnpackException
          context "on non-existent files" $
            it "should fail" $
              (runResourceT $
                 do (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                    unpack (ArchiveFilePath "/does/not/exist.zip") tmpDir) `shouldThrow` anyIOException
     describe "unpackerFor" $
       do context "on files with a \"rar\" extension" $
            do it "should unpack a.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do Just unp <- unpackerFor rf
                         (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unp rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do Just unp <- unpackerFor rf
                         (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unp rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do Just unp <- unpackerFor rf
                         (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unp rf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          context "on files with a \"zip\" extension" $
            do it "should unpack a zip file to a temporary directory" $
                 do zf <- zipFile
                    runResourceT $
                      do Just unp <- unpackerFor zf
                         (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unp zf tmpDir
                         sysTmpDir <- liftIO getTemporaryDirectory
                         liftIO $ isPrefixOf sysTmpDir tmpDir `shouldBe` True
               it "should unpack a.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do Just unp <- unpackerFor zf
                         (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unp zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "a.txt") `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do Just unp <- unpackerFor zf
                         (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unp zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "b.txt") `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do Just unp <- unpackerFor zf
                         (_, tmpDir) <- createSystemTempDirectory "UnpackSpec"
                         unp zf tmpDir
                         liftIO $ doesFileExist (tmpDir </> "foo" </> "c.txt") `shouldReturn` True
          context "on unsupported archive types" $
            it "should return 'Nothing'" $
              do unsupported <- liftIO unsupportedFile
                 isNothing <$> unpackerFor unsupported `shouldReturn` True
          -- context "on files with no extension" $
          --   it "should return 'Nothing'" $
          --     isNothing <$> unpackerFor (ArchiveFilePath "foo/rar/not-a-zip") `shouldReturn` True
