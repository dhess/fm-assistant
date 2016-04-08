{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.FMAssistant.UnpackSpec
       ( spec
       ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Path ((</>), mkAbsFile, mkRelFile, parseAbsFile)
import Path.IO (doesFileExist)
import Test.Hspec
import Paths_fm_assistant

import Game.FMAssistant.Types (ArchiveFilePath(..), UnpackDirPath(..))
import Game.FMAssistant.Unpack
import Game.FMAssistant.Util (createSystemTempDir)

getArchiveFilePath :: FilePath -> IO ArchiveFilePath
getArchiveFilePath fp =
  do fn <- getDataFileName fp
     absfn <- parseAbsFile fn
     return $ ArchiveFilePath absfn

zipFile :: IO ArchiveFilePath
zipFile = getArchiveFilePath "data/test/test.zip"

damagedZipFile :: IO ArchiveFilePath
damagedZipFile = getArchiveFilePath "data/test/damaged-test.zip"

rarFile :: IO ArchiveFilePath
rarFile = getArchiveFilePath "data/test/test.rar"

damagedRarFile :: IO ArchiveFilePath
damagedRarFile = getArchiveFilePath "data/test/damaged-test.rar"

anyUnpackException :: Selector UnpackException
anyUnpackException = const True

spec :: Spec
spec =
  do describe "unpackZip" $
       do it "should unpack a.txt from the test file" $
            do zf <- zipFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                    unpackZip zf (UnpackDirPath tmpDir)
                    liftIO $ doesFileExist (tmpDir </> $(mkRelFile "a.txt")) `shouldReturn` True
          it "should unpack b.txt from the test file" $
            do zf <- zipFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                    unpackZip zf (UnpackDirPath tmpDir)
                    liftIO $ doesFileExist (tmpDir </> $(mkRelFile "b.txt")) `shouldReturn` True
          it "should unpack c.txt from the test file" $
            do zf <- zipFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                    unpackZip zf (UnpackDirPath tmpDir)
                    liftIO $ doesFileExist (tmpDir </> $(mkRelFile "foo/c.txt")) `shouldReturn` True
          it "should fail when used on a damaged ZIP file" $
            (runResourceT $
               do zf <- liftIO damagedZipFile
                  (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                  unpackZip zf (UnpackDirPath tmpDir)) `shouldThrow` anyUnpackException
          it "should fail when used on a RAR file" $
            (runResourceT $
               do rf <- liftIO rarFile
                  (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                  unpackZip rf (UnpackDirPath tmpDir)) `shouldThrow` anyUnpackException
          it "should fail when the file doesn't exist" $
            (runResourceT $
               do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                  unpackZip (ArchiveFilePath $(mkAbsFile "/does/not/exist.zip")) (UnpackDirPath tmpDir)) `shouldThrow` anyUnpackException
     describe "unpackRar" $
       do it "should unpack a.txt from the test file" $
            do rf <- rarFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                    unpackRar rf (UnpackDirPath tmpDir)
                    liftIO $ doesFileExist (tmpDir </> $(mkRelFile "a.txt")) `shouldReturn` True
          it "should unpack b.txt from the test file" $
            do rf <- rarFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                    unpackRar rf (UnpackDirPath tmpDir)
                    liftIO $ doesFileExist (tmpDir </> $(mkRelFile "b.txt")) `shouldReturn` True
          it "should unpack c.txt from the test file" $
            do rf <- rarFile
               runResourceT $
                 do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                    unpackRar rf (UnpackDirPath tmpDir)
                    liftIO $ doesFileExist (tmpDir </> $(mkRelFile "foo/c.txt")) `shouldReturn` True
          it "should fail when used on a damaged RAR file" $
            (runResourceT $
               do rf <- liftIO damagedRarFile
                  (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                  unpackRar rf (UnpackDirPath tmpDir)) `shouldThrow` anyUnpackException
          it "should fail when used on a ZIP file" $
            (runResourceT $
               do zf <- liftIO zipFile
                  (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                  unpackRar zf (UnpackDirPath tmpDir)) `shouldThrow` anyUnpackException
          it "should fail when the file doesn't exist" $
            (runResourceT $
               do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                  unpackRar (ArchiveFilePath $(mkAbsFile "/does/not/exist.rar")) (UnpackDirPath tmpDir)) `shouldThrow` anyUnpackException
     describe "unpack" $
       do context "on RAR files" $
            do it "should unpack a.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                         unpack rf (UnpackDirPath tmpDir)
                         liftIO $ doesFileExist (tmpDir </> $(mkRelFile "a.txt")) `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                         unpack rf (UnpackDirPath tmpDir)
                         liftIO $ doesFileExist (tmpDir </> $(mkRelFile "b.txt")) `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do rf <- rarFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                         unpack rf (UnpackDirPath tmpDir)
                         liftIO $ doesFileExist (tmpDir </> $(mkRelFile "foo/c.txt")) `shouldReturn` True
               it "should fail when used on a damaged file" $
                 (runResourceT $
                    do rf <- liftIO damagedRarFile
                       (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                       unpack rf (UnpackDirPath tmpDir)) `shouldThrow` anyUnpackException
          context "on ZIP files" $
            do it "should unpack a.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                         unpack zf (UnpackDirPath tmpDir)
                         liftIO $ doesFileExist (tmpDir </> $(mkRelFile "a.txt")) `shouldReturn` True
               it "should unpack b.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                         unpack zf (UnpackDirPath tmpDir)
                         liftIO $ doesFileExist (tmpDir </> $(mkRelFile "b.txt")) `shouldReturn` True
               it "should unpack foo/c.txt from the test file" $
                 do zf <- zipFile
                    runResourceT $
                      do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                         unpack zf (UnpackDirPath tmpDir)
                         liftIO $ doesFileExist (tmpDir </> $(mkRelFile "foo/c.txt")) `shouldReturn` True
               it "should fail when used on a damaged file" $
                 (runResourceT $
                    do zf <- liftIO damagedZipFile
                       (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                       unpack zf (UnpackDirPath tmpDir)) `shouldThrow` anyUnpackException
          context "on non-existent files" $
            it "should fail" $
              (runResourceT $
                 do (_, tmpDir) <- createSystemTempDir "UnpackSpec"
                    unpack (ArchiveFilePath $(mkAbsFile "/does/not/exist.zip")) (UnpackDirPath tmpDir)) `shouldThrow` anyIOException
