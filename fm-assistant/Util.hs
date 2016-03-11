{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
       ( anyFailure
       , catchesMost
       , catchesMostQuietly
       ) where

import Control.Exception (IOException)
import Control.Monad.Catch (Handler(..), catches)
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

handleUnpackQuietly :: UnpackException -> IO ()
handleUnpackQuietly (UnsupportedArchive (ArchiveFilePath fp)) =
  putStrLn fp
handleUnpackQuietly (UnzipError (ArchiveFilePath fp) _ _) =
  putStrLn fp
handleUnpackQuietly (UnrarError (ArchiveFilePath fp) _ _) =
  putStrLn fp

handleKitPackQuietly :: KitPackException -> IO ()
handleKitPackQuietly (NoSuchUserDirectory (UserDirFilePath fp)) =
  putStrLn fp
handleKitPackQuietly (EmptyArchive (ArchiveFilePath fp)) =
  putStrLn fp
handleKitPackQuietly (SingleFileArchive (ArchiveFilePath fp)) =
  putStrLn fp
handleKitPackQuietly (MultipleFilesOrDirectories (ArchiveFilePath fp)) =
  putStrLn fp

catchesMostQuietly :: IO () -> IO ()
catchesMostQuietly action =
  catches action most
  where
    most = [Handler handleIOQuietly, Handler handleUnpackQuietly, Handler handleKitPackQuietly]
