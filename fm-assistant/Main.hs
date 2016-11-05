{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

import qualified CutoutFacesMegapack (run)
import qualified CutoutFacesIcons (run)
import qualified DebskisHairstyles (run)
import qualified KitPack (run)
import qualified MetallicLogos (run)
import qualified Mod (Command, run, parser)
import qualified RealNamesFix16 (run)
import qualified Repack (Command, parser)
import qualified RepackMultiple (Command, parser)
import qualified Skin (run)

data GlobalOptions =
  GlobalOptions {_cmd :: Command}

data Command
  = DebskisHairstyles Repack.Command
  | KitPack RepackMultiple.Command
  | CutoutFacesMegapack Repack.Command
  | CutoutFacesIcons Repack.Command
  | MetallicLogos Repack.Command
  | Mod Mod.Command
  | RealNamesFix16 Repack.Command
  | Skin RepackMultiple.Command

debskisHairstylesCmd :: Parser Command
debskisHairstylesCmd = DebskisHairstyles <$> Repack.parser

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

realNamesFix16Cmd :: Parser Command
realNamesFix16Cmd = RealNamesFix16 <$> Repack.parser

skinCmd :: Parser Command
skinCmd = Skin <$> RepackMultiple.parser

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  hsubparser
    (command "debskis-hairstyles" (info debskisHairstylesCmd (progDesc "Debski's World of Hairstyles commands")) <>
     command "kitpack" (info kitPackCmd (progDesc "Kit pack commands")) <>
     command "cutout-faces-megapack" (info cutoutFacesMegapackCmd (progDesc "Sortioutsi Cutout Megapack commands")) <>
     command "cutout-faces-icons" (info cutoutFacesIconsCmd (progDesc "Sortioutsi Cutout Icons commands")) <>
     command "metallic-logos" (info metallicLogosCmd (progDesc "Metallic Logos commands")) <>
     command "real-names-fix-16" (info realNamesFix16Cmd (progDesc "Sortitoutsi Real Names Fix (for FM16) commands")) <>
     command "skin" (info skinCmd (progDesc "Skin commands")) <>
     command "mod" (info modCmd (progDesc "Mod pack commands")))

run :: GlobalOptions -> IO ExitCode
run (GlobalOptions (DebskisHairstyles cmd)) = DebskisHairstyles.run cmd
run (GlobalOptions (KitPack cmd)) = KitPack.run cmd
run (GlobalOptions (CutoutFacesMegapack cmd)) = CutoutFacesMegapack.run cmd
run (GlobalOptions (CutoutFacesIcons cmd)) = CutoutFacesIcons.run cmd
run (GlobalOptions (MetallicLogos cmd)) = MetallicLogos.run cmd
run (GlobalOptions (Mod cmd)) = Mod.run cmd
run (GlobalOptions (RealNamesFix16 cmd)) = RealNamesFix16.run cmd
run (GlobalOptions (Skin cmd)) = Skin.run cmd

main :: IO ExitCode
main = execParser opts >>= run >>= exitWith
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Manage Football Manager mods" <>
                header "fm-assistant")
