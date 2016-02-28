{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Game.FMAssistant.Streaming
import Options.Applicative
import System.IO.Temp (withSystemTempDirectory)

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Install InstallOptions

data InstallOptions =
  InstallOptions {fileName :: FilePath}

installCmd :: Parser Command
installCmd = Install <$> installOptions

installOptions :: Parser InstallOptions
installOptions =
  InstallOptions <$>
    argument str (metavar "FILE")

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  switch (long "quiet" <>
          short 'q' <>
          help "Be quiet") <*>
  flag Normal
       Verbose
       (long "verbose" <>
        short 'v' <>
        help "Enable verbose mode") <*>
  hsubparser
    (command "install" (info installCmd (progDesc "Install a mod")))

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Install (InstallOptions fn))) =
  withSystemTempDirectory
  "fm-assistant"
  (\tmpDir ->
    do putStrLn $ "Unpacking " ++ fn ++ " to " ++ tmpDir
       unpack tmpDir fn
       threadDelay $ 10 * 1000000)
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Install Football Manager mods" <>
                header "fm-assistant")
