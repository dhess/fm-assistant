module Main where

import System.FilePath ((</>))
import Test.DocTest

addPrefix :: FilePath -> FilePath
addPrefix fp = "src" </> "Game" </> "FMAssistant" </> fp

testFiles :: [FilePath]
testFiles =
  map addPrefix
      ["Magic.hs"
      ,"Mod.hs"
      ,"Repack.hs"
      ,"Repack" </> "DebskisHairstyles.hs"
      ,"Repack" </> "Faces.hs"
      ,"Repack" </> "Internal.hs"
      ,"Repack" </> "Kits.hs"
      ,"Repack" </> "MetallicLogos.hs"
      ,"Repack" </> "RealNamesFix.hs"
      ,"Repack" </> "Unpack.hs"
      ,"Streaming.hs"
      ,"Types.hs"
      ,"Util.hs"
      ]

main :: IO ()
main = doctest testFiles
