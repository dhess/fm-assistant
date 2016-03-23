module Main where

import System.FilePath ((</>))
import Test.DocTest

addPrefix :: FilePath -> FilePath
addPrefix fp = "src" </> "Game" </> "FMAssistant" </> fp

testFiles :: [FilePath]
testFiles =
  map addPrefix
      ["Install.hs", "Magic.hs","Mod" </> "Kits.hs","Streaming.hs","Types.hs","Unpack.hs","Util.hs","Version.hs"]

main :: IO ()
main = doctest testFiles
