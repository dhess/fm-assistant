{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
       ( anyFailure
       , handleIO
       , handleIOQuietly
       ) where

import Control.Exception (IOException)
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
