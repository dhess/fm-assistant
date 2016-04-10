module Main where

import System.FilePath ((</>))
import Test.DocTest

addPrefix :: FilePath -> FilePath
addPrefix fp = "src" </> "Game" </> "FMAssistant" </> fp

testFiles :: [FilePath]
testFiles =
  map addPrefix
      ["Install.hs", "Magic.hs","Streaming.hs","Types.hs","Repack" </> "Unpack.hs","Util.hs"]

main :: IO ()
main = doctest testFiles
