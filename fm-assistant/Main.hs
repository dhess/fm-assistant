{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

import qualified CutoutFacesMegapack (Command, run, parser)
import qualified CutoutFacesIcons (Command, run, parser)
import qualified KitPack (Command, run, parser)
import qualified Mod (Command, run, parser)

data GlobalOptions =
  GlobalOptions {_cmd :: Command}

data Command
  = KitPack KitPack.Command
  | CutoutFacesMegapack CutoutFacesMegapack.Command
  | CutoutFacesIcons CutoutFacesIcons.Command
  | Mod Mod.Command

kitPackCmd :: Parser Command
kitPackCmd = KitPack <$> KitPack.parser

cutoutFacesMegapackCmd :: Parser Command
cutoutFacesMegapackCmd = CutoutFacesMegapack <$> CutoutFacesMegapack.parser

cutoutFacesIconsCmd :: Parser Command
cutoutFacesIconsCmd = CutoutFacesIcons <$> CutoutFacesIcons.parser

modCmd :: Parser Command
modCmd = Mod <$> Mod.parser

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  hsubparser
    (command "kitpack" (info kitPackCmd (progDesc "Kit pack commands")) <>
     command "cutout-faces-megapack" (info cutoutFacesMegapackCmd (progDesc "Sortioutsi Cutout Megapack commands")) <>
     command "cutout-faces-icons" (info cutoutFacesIconsCmd (progDesc "Sortioutsi Cutout Icons commands")) <>
     command "mod" (info modCmd (progDesc "Mod pack commands")))

run :: GlobalOptions -> IO ExitCode
run (GlobalOptions (KitPack cmd)) = KitPack.run cmd
run (GlobalOptions (CutoutFacesMegapack cmd)) = CutoutFacesMegapack.run cmd
run (GlobalOptions (CutoutFacesIcons cmd)) = CutoutFacesIcons.run cmd
run (GlobalOptions (Mod cmd)) = Mod.run cmd

main :: IO ExitCode
main = execParser opts >>= run >>= exitWith
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Manage Football Manager mods" <>
                header "fm-assistant")
