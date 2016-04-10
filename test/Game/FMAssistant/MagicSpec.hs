{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.MagicSpec
       ( spec
       ) where

import Path (Path, Abs, File, mkAbsFile, parseAbsFile)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Magic

getPath :: FilePath -> IO (Path Abs File)
getPath fp =
  do fn <- getDataFileName fp
     parseAbsFile fn

unsupportedFile :: IO (Path Abs File)
unsupportedFile = getPath "data/test/test.tar"

zipFile :: IO (Path Abs File)
zipFile = getPath "data/test/test.zip"

damagedZipFile :: IO (Path Abs File)
damagedZipFile = getPath "data/test/damaged-test.zip"

zipFileWithRarExtension :: IO (Path Abs File)
zipFileWithRarExtension = getPath "data/test/test_is_actually_zip.rar"

zipFileWithNoExtension :: IO (Path Abs File)
zipFileWithNoExtension = getPath "data/test/test_zip"

rarFile :: IO (Path Abs File)
rarFile = getPath "data/test/test.rar"

damagedRarFile :: IO (Path Abs File)
damagedRarFile = getPath "data/test/damaged-test.rar"

rarFileWithZipExtension :: IO (Path Abs File)
rarFileWithZipExtension = getPath "data/test/test_is_actually_rar.zip"

rarFileWithNoExtension :: IO (Path Abs File)
rarFileWithNoExtension = getPath "data/test/test_rar"

spec :: Spec
spec =
  do describe "isZipFile" $
       do it "should recognize ZIP archives" $
            do zf <- zipFile
               isZipFile zf `shouldReturn` True
          it "should recognize (some) damaged ZIP archives" $
            do dzf <- damagedZipFile
               isZipFile dzf `shouldReturn` True
          it "should recognize ZIP archives with the wrong extension" $
            do zf <- zipFileWithRarExtension
               isZipFile zf `shouldReturn` True
          it "should recognize ZIP archives with no extension" $
            do zf <- zipFileWithNoExtension
               isZipFile zf `shouldReturn` True
          it "should not recognize RAR archives" $
            do rf <- rarFile
               isZipFile rf `shouldReturn` False
          it "should not recognize RAR archives with a ZIP extension" $
            do rf <- rarFileWithZipExtension
               isZipFile rf `shouldReturn` False
          it "should not recognize RAR archives with no extension" $
            do rf <- rarFileWithNoExtension
               isZipFile rf `shouldReturn` False
          it "should not recognize unsupported archive types" $
            do tf <- unsupportedFile
               isZipFile tf `shouldReturn` False
     describe "isRarFile" $
       do it "should recognize RAR archives" $
            do rf <- rarFile
               isRarFile rf `shouldReturn` True
          it "should recognize (some) damaged RAR archives" $
            do drf <- damagedRarFile
               isRarFile drf `shouldReturn` True
          it "should recognize RAR archives with the wrong extension" $
            do rf <- rarFileWithZipExtension
               isRarFile rf `shouldReturn` True
          it "should recognize RAR archives with no extension" $
            do rf <- rarFileWithNoExtension
               isRarFile rf `shouldReturn` True
          it "should not recognize ZIP archives" $
            do zf <- zipFile
               isRarFile zf `shouldReturn` False
          it "should not recognize ZIP archives with a RAR extension" $
            do zf <- zipFileWithRarExtension
               isRarFile zf `shouldReturn` False
          it "should not recognize ZIP archives with no extension" $
            do rf <- zipFileWithNoExtension
               isRarFile rf `shouldReturn` False
          it "should not recognize unsupported archive types" $
            do tf <- unsupportedFile
               isRarFile tf `shouldReturn` False
     describe "identifyFile" $
       do it "should recognize RAR archives" $
            do rf <- rarFile
               identifyFile rf `shouldReturn` Just Rar
          it "should recognize (some) damaged RAR archives" $
            do drf <- damagedRarFile
               identifyFile drf `shouldReturn` Just Rar
          it "should recognize RAR archives with the wrong extension" $
            do rf <- rarFileWithZipExtension
               identifyFile rf `shouldReturn` Just Rar
          it "should recognize RAR archives with no extension" $
            do rf <- rarFileWithNoExtension
               identifyFile rf `shouldReturn` Just Rar
          it "should recognize ZIP archives" $
            do zf <- zipFile
               identifyFile zf `shouldReturn` Just Zip
          it "should recognize (some) damaged ZIP archives" $
            do dzf <- damagedZipFile
               identifyFile dzf `shouldReturn` Just Zip
          it "should recognize ZIP archives with the wrong extension" $
            do zf <- zipFileWithRarExtension
               identifyFile zf `shouldReturn` Just Zip
          it "should recognize ZIP archives with no extension" $
            do zf <- zipFileWithNoExtension
               identifyFile zf `shouldReturn` Just Zip
          it "should not recognize unsupported archive types" $
            do tf <- unsupportedFile
               identifyFile tf `shouldReturn` Nothing
          it "should fail when the file doesn't exist" $
            identifyFile $(mkAbsFile "/this/does/not/exist.zip") `shouldThrow` anyIOException
