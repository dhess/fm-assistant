{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Game.FMAssistant.MagicSpec
       ( spec
       ) where

import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Types (ArchiveFilePath(..))
import Game.FMAssistant.Magic

unsupportedFile :: IO ArchiveFilePath
unsupportedFile = ArchiveFilePath <$> getDataFileName "data/test/test.tar"

zipFile :: IO ArchiveFilePath
zipFile = ArchiveFilePath <$> getDataFileName "data/test/test.zip"

damagedZipFile :: IO ArchiveFilePath
damagedZipFile = ArchiveFilePath <$> getDataFileName "data/test/damaged-test.zip"

zipFileWithRarExtension :: IO ArchiveFilePath
zipFileWithRarExtension = ArchiveFilePath <$> getDataFileName "data/test/test_is_actually_zip.rar"

zipFileWithNoExtension :: IO ArchiveFilePath
zipFileWithNoExtension = ArchiveFilePath <$> getDataFileName "data/test/test_zip"

rarFile :: IO ArchiveFilePath
rarFile = ArchiveFilePath <$> getDataFileName "data/test/test.rar"

damagedRarFile :: IO ArchiveFilePath
damagedRarFile = ArchiveFilePath <$> getDataFileName "data/test/damaged-test.rar"

rarFileWithZipExtension :: IO ArchiveFilePath
rarFileWithZipExtension = ArchiveFilePath <$> getDataFileName "data/test/test_is_actually_rar.zip"

rarFileWithNoExtension :: IO ArchiveFilePath
rarFileWithNoExtension = ArchiveFilePath <$> getDataFileName "data/test/test_rar"

spec :: Spec
spec =
  do describe "isZipArchive" $
       do it "should recognize ZIP archives" $
            do zf <- zipFile
               isZipArchive zf `shouldReturn` True
          it "should recognize (some) damaged ZIP archives" $
            do dzf <- damagedZipFile
               isZipArchive dzf `shouldReturn` True
          it "should recognize ZIP archives with the wrong extension" $
            do zf <- zipFileWithRarExtension
               isZipArchive zf `shouldReturn` True
          it "should recognize ZIP archives with no extension" $
            do zf <- zipFileWithNoExtension
               isZipArchive zf `shouldReturn` True
          it "should not recognize RAR archives" $
            do rf <- rarFile
               isZipArchive rf `shouldReturn` False
          it "should not recognize RAR archives with a ZIP extension" $
            do rf <- rarFileWithZipExtension
               isZipArchive rf `shouldReturn` False
          it "should not recognize RAR archives with no extension" $
            do rf <- rarFileWithNoExtension
               isZipArchive rf `shouldReturn` False
          it "should not recognize unsupported archive types" $
            do tf <- unsupportedFile
               isZipArchive tf `shouldReturn` False
     describe "isRarArchive" $
       do it "should recognize RAR archives" $
            do rf <- rarFile
               isRarArchive rf `shouldReturn` True
          it "should recognize (some) damaged RAR archives" $
            do drf <- damagedRarFile
               isRarArchive drf `shouldReturn` True
          it "should recognize RAR archives with the wrong extension" $
            do rf <- rarFileWithZipExtension
               isRarArchive rf `shouldReturn` True
          it "should recognize RAR archives with no extension" $
            do rf <- rarFileWithNoExtension
               isRarArchive rf `shouldReturn` True
          it "should not recognize ZIP archives" $
            do zf <- zipFile
               isRarArchive zf `shouldReturn` False
          it "should not recognize ZIP archives with a RAR extension" $
            do zf <- zipFileWithRarExtension
               isRarArchive zf `shouldReturn` False
          it "should not recognize ZIP archives with no extension" $
            do rf <- zipFileWithNoExtension
               isRarArchive rf `shouldReturn` False
          it "should not recognize unsupported archive types" $
            do tf <- unsupportedFile
               isRarArchive tf `shouldReturn` False
     describe "identifyArchive" $
       do it "should recognize RAR archives" $
            do rf <- rarFile
               identifyArchive rf `shouldReturn` Just Rar
          it "should recognize (some) damaged RAR archives" $
            do drf <- damagedRarFile
               identifyArchive drf `shouldReturn` Just Rar
          it "should recognize RAR archives with the wrong extension" $
            do rf <- rarFileWithZipExtension
               identifyArchive rf `shouldReturn` Just Rar
          it "should recognize RAR archives with no extension" $
            do rf <- rarFileWithNoExtension
               identifyArchive rf `shouldReturn` Just Rar
          it "should recognize ZIP archives" $
            do zf <- zipFile
               identifyArchive zf `shouldReturn` Just Zip
          it "should recognize (some) damaged ZIP archives" $
            do dzf <- damagedZipFile
               identifyArchive dzf `shouldReturn` Just Zip
          it "should recognize ZIP archives with the wrong extension" $
            do zf <- zipFileWithRarExtension
               identifyArchive zf `shouldReturn` Just Zip
          it "should recognize ZIP archives with no extension" $
            do zf <- zipFileWithNoExtension
               identifyArchive zf `shouldReturn` Just Zip
          it "should not recognize unsupported archive types" $
            do tf <- unsupportedFile
               identifyArchive tf `shouldReturn` Nothing
          it "should fail when the file doesn't exist" $
            identifyArchive (ArchiveFilePath "/this/does/not/exist.zip") `shouldThrow` anyIOException
