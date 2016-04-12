{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

import qualified CutoutFacesMegapack (run)
import qualified CutoutFacesIcons (run)
import qualified KitPack (run)
import qualified MetallicLogos (run)
import qualified Mod (Command, run, parser)
import qualified RealNamesFix (run)
import qualified Repack (Command, parser)
import qualified RepackMultiple (Command, parser)

data GlobalOptions =
  GlobalOptions {_cmd :: Command}

data Command
  = KitPack RepackMultiple.Command
  | CutoutFacesMegapack Repack.Command
  | CutoutFacesIcons Repack.Command
  | MetallicLogos Repack.Command
  | Mod Mod.Command
  | RealNamesFix Repack.Command

kitPackCmd :: Parser Command
kitPackCmd = KitPack <$> RepackMultiple.parser

cutoutFacesMegapackCmd :: Parser Command
cutoutFacesMegapackCmd = CutoutFacesMegapack <$> Repack.parser

cutoutFacesIconsCmd :: Parser Command
cutoutFacesIconsCmd = CutoutFacesIcons <$> Repack.parser

metallicLogosCmd :: Parser Command
metallicLogosCmd = MetallicLogos <$> Repack.parser

modCmd :: Parser Command
modCmd = Mod <$> Mod.parser

realNamesFixCmd :: Parser Command
realNamesFixCmd = RealNamesFix <$> Repack.parser

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  hsubparser
    (command "kitpack" (info kitPackCmd (progDesc "Kit pack commands")) <>
     command "cutout-faces-megapack" (info cutoutFacesMegapackCmd (progDesc "Sortioutsi Cutout Megapack commands")) <>
     command "cutout-faces-icons" (info cutoutFacesIconsCmd (progDesc "Sortioutsi Cutout Icons commands")) <>
     command "metallic-logos" (info metallicLogosCmd (progDesc "Metallic Logos commands")) <>
     command "real-names-fix" (info realNamesFixCmd (progDesc "Sortitoutsi Real Names Fix commands")) <>
     command "mod" (info modCmd (progDesc "Mod pack commands")))

run :: GlobalOptions -> IO ExitCode
run (GlobalOptions (KitPack cmd)) = KitPack.run cmd
run (GlobalOptions (CutoutFacesMegapack cmd)) = CutoutFacesMegapack.run cmd
run (GlobalOptions (CutoutFacesIcons cmd)) = CutoutFacesIcons.run cmd
run (GlobalOptions (MetallicLogos cmd)) = MetallicLogos.run cmd
run (GlobalOptions (Mod cmd)) = Mod.run cmd
run (GlobalOptions (RealNamesFix cmd)) = RealNamesFix.run cmd

main :: IO ExitCode
main = execParser opts >>= run >>= exitWith
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Manage Football Manager mods" <>
                header "fm-assistant")
