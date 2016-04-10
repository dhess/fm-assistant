{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
       ( anyFailure
       , catchesMost
       , catchesMostQuietly
       , handleFME
       , handleIO
       , handleFMEQuietly
       , handleIOQuietly
       ) where

import Control.Exception (IOException)
import Control.Monad.Catch (Handler(..), catches)
import Game.FMAssistant.Types (SomeFMAssistantException)
import System.Exit (ExitCode(..))
import System.IO (hPrint, stderr)

anyFailure :: ExitCode -> [ExitCode] -> ExitCode
anyFailure failCode codes =
  if all (ExitSuccess ==) codes
     then ExitSuccess
     else failCode

catchesMost :: IO ExitCode -> IO ExitCode
catchesMost act =
  catches act most
  where
    most = [Handler handleIO, Handler handleFME]

catchesMostQuietly :: FilePath -> IO () -> IO ()
catchesMostQuietly fp act =
  catches act most
  where
    most = [Handler (handleIOQuietly fp), Handler (handleIOQuietly fp)]

handleFME :: SomeFMAssistantException -> IO ExitCode
handleFME e = hPrint stderr e >> return (ExitFailure 2)

handleIO :: IOException -> IO ExitCode
handleIO e = hPrint stderr e >> return (ExitFailure 1)

-- | "Quietly" variants print only the given 'FilePath', and don't
-- return an 'ExitCode'. This is useful when using the command to list
-- files which satisfy some failure predicate (say, invalid kit
-- packs), so that we can pipe the list of files in the shell to
-- another command without indicating failure.
--
-- They also output to stdout, for the above reason.

handleFMEQuietly :: FilePath -> SomeFMAssistantException -> IO ()
handleFMEQuietly fp _ = putStrLn fp

handleIOQuietly :: FilePath -> IOException -> IO ()
handleIOQuietly fp _ = putStrLn fp
