{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

import qualified CutoutFaces (Command, run, parser)
import qualified KitPack (Command, run, parser)
import qualified Mod (Command, run, parser)

data GlobalOptions =
  GlobalOptions {_cmd :: Command}

data Command
  = KitPack KitPack.Command
  | CutoutFaces CutoutFaces.Command
  | Mod Mod.Command

kitPackCmd :: Parser Command
kitPackCmd = KitPack <$> KitPack.parser

cutoutFacesCmd :: Parser Command
cutoutFacesCmd = CutoutFaces <$> CutoutFaces.parser

modCmd :: Parser Command
modCmd = Mod <$> Mod.parser

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  hsubparser
    (command "kitpack" (info kitPackCmd (progDesc "Kit pack commands")) <>
     command "cutout-faces" (info cutoutFacesCmd (progDesc "Sortioutsi Cutout faces commands")) <>
     command "mod" (info modCmd (progDesc "Mod pack commands")))

run :: GlobalOptions -> IO ExitCode
run (GlobalOptions (KitPack cmd)) = KitPack.run cmd
run (GlobalOptions (CutoutFaces cmd)) = CutoutFaces.run cmd
run (GlobalOptions (Mod cmd)) = Mod.run cmd

main :: IO ExitCode
main = execParser opts >>= run >>= exitWith
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Manage Football Manager mods" <>
                header "fm-assistant")
