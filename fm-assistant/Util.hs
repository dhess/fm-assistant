{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
       ( anyFailure
       , catchesMost
       , catchesMostQuietly
       , toFpList
       ) where

import Prelude hiding (FilePath)
import Control.Exception (IOException)
import Control.Monad.Catch (Handler(..), catches)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Filesystem (decodeString, encodeString)
import Game.FMAssistant.Mod.Kits (KitPackException(..))
import Game.FMAssistant.Types (ArchiveFilePath(..), UserDirFilePath(..))
import Game.FMAssistant.Unpack (UnpackException(..))
import System.Exit (ExitCode(..))
import System.IO (hPrint, stderr)
import System.IO.Error (ioeGetFileName)

anyFailure :: ExitCode -> [ExitCode] -> ExitCode
anyFailure failCode codes =
  if all (ExitSuccess ==) codes
     then ExitSuccess
     else failCode

handleIO :: IOException -> IO ExitCode
handleIO e = hPrint stderr e >> return (ExitFailure 1)

handleUnpack :: UnpackException -> IO ExitCode
handleUnpack e = hPrint stderr e >> return (ExitFailure 2)

handleKitPack :: KitPackException -> IO ExitCode
handleKitPack e = hPrint stderr e >> return (ExitFailure 3)

catchesMost :: IO ExitCode -> IO ExitCode
catchesMost action =
  catches action most
  where
    most = [Handler handleIO, Handler handleUnpack, Handler handleKitPack]

-- | "Quietly" variants print only the filename associated with the
-- exception, and don't return an 'ExitCode'. This is useful when
-- using the command to list files which satisfy some failure
-- predicate (say, invalid kit packs), so that we can pipe the list of
-- files in the shell to another command without indicating failure.
--
-- They also output to stdout, for the above reason.

handleIOQuietly :: IOException -> IO ()
handleIOQuietly e =
  case ioeGetFileName e of
    Just fn -> putStrLn fn
    _ -> return () -- Probably an error, pass on it for now.

putFpLn :: FilePath -> IO ()
putFpLn = putStrLn . Filesystem.encodeString

handleUnpackQuietly :: UnpackException -> IO ()
handleUnpackQuietly (UnsupportedArchive (ArchiveFilePath fp)) =
  putFpLn fp
handleUnpackQuietly (UnzipError (ArchiveFilePath fp) _ _) =
  putFpLn fp
handleUnpackQuietly (UnrarError (ArchiveFilePath fp) _ _) =
  putFpLn fp

handleKitPackQuietly :: KitPackException -> IO ()
handleKitPackQuietly (NoSuchUserDirectory (UserDirFilePath fp)) =
  putFpLn fp
handleKitPackQuietly (EmptyArchive (ArchiveFilePath fp)) =
  putFpLn fp
handleKitPackQuietly (SingleFileArchive (ArchiveFilePath fp)) =
  putFpLn fp
handleKitPackQuietly (MultipleFilesOrDirectories (ArchiveFilePath fp)) =
  putFpLn fp

catchesMostQuietly :: IO () -> IO ()
catchesMostQuietly action =
  catches action most
  where
    most = [Handler handleIOQuietly, Handler handleUnpackQuietly, Handler handleKitPackQuietly]

toFpList :: [String] -> [FilePath]
toFpList = map Filesystem.decodeString
