module Main where

import Test.DocTest

addPrefix :: FilePath -> FilePath
addPrefix fp = "src/Game/FMAssistant/" ++ fp

testFiles :: [FilePath]
testFiles =
  map addPrefix
      ["FM16.hs","Mod/Kits.hs","Streaming.hs","Types.hs","Unpack.hs","Util.hs"]

main :: IO ()
main = doctest testFiles
