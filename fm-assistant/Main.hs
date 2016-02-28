module Main where

import Game.FMAssistant.Actions (greet)
import Options.Applicative

data Hello =
  Hello {greeting :: String
        ,quiet :: Bool
        ,target :: String}

helloParser :: Parser Hello
helloParser =
  Hello <$>
  strOption (long "greeting" <>
             short 'g' <>
             value "Hello" <>
             metavar "GREETING" <>
             help "The greeting") <*>
  switch (long "quiet" <>
          short 'q' <>
          help "Be quiet") <*>
  argument str (metavar "TARGET")


run :: Hello -> IO ()
run (Hello g False t) = greet g t
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> helloParser)
               (fullDesc <>
                progDesc "Print a greeting for TARGET" <>
                header "fm-assistant - a command-line interface for fm-assistant")
