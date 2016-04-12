{-|
Module      : Game.FMAssistant.Repack
Description : Repacking mods for use with fm-assistant
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

module Game.FMAssistant.Repack
       ( -- Legacy mod archive files
         ArchiveFilePath(..)
       , archiveName
         -- * Repacking actions
       , repackCutoutIcons
       , repackCutoutMegapack
       , repackKitPack
       , repackMetallicLogos
       , repackRealNamesFix
         -- * Mod-specific repacking exceptions
       , FacePackRepackException(..)
       , MetallicLogosRepackException(..)
       , RealNamesFixRepackException(..)
         -- * General repacking exceptions
       , RepackException(..)
       ) where

import Game.FMAssistant.Repack.Faces
       (FacePackRepackException(..), repackCutoutIcons, repackCutoutMegapack)
import Game.FMAssistant.Repack.Internal
       (ArchiveFilePath(..), RepackException(..), archiveName)
import Game.FMAssistant.Repack.Kits (repackKitPack)
import Game.FMAssistant.Repack.MetallicLogos (MetallicLogosRepackException(..), repackMetallicLogos)
import Game.FMAssistant.Repack.RealNamesFix (RealNamesFixRepackException(..), repackRealNamesFix)
