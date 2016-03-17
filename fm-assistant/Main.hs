{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

import qualified CutoutMegapack (Command, run, parser)
import qualified KitPack (Command, run, parser)

data GlobalOptions =
  GlobalOptions {_cmd :: Command}

data Command
  = KitPack KitPack.Command
  | CutoutMegapack CutoutMegapack.Command

kitPackCmd :: Parser Command
kitPackCmd = KitPack <$> KitPack.parser

cutoutMegapackCmd :: Parser Command
cutoutMegapackCmd = CutoutMegapack <$> CutoutMegapack.parser

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  hsubparser
    (command "kitpack" (info kitPackCmd (progDesc "Kit pack commands")) <>
     command "cutoutmegapack" (info cutoutMegapackCmd (progDesc "Sortioutsi Cutout Megapack commands")))

run :: GlobalOptions -> IO ExitCode
run (GlobalOptions (KitPack cmd)) = KitPack.run cmd
run (GlobalOptions (CutoutMegapack cmd)) = CutoutMegapack.run cmd

main :: IO ExitCode
main = execParser opts >>= run >>= exitWith
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Manage Football Manager mods" <>
                header "fm-assistant")
