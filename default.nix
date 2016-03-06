{ mkDerivation, base, bytestring, directory, doctest, exceptions
, foldl, hspec, managed, mtl, optparse-applicative, stdenv
, streaming, streaming-bytestring, system-filepath, tar, temporary
, text, transformers, turtle
}:
mkDerivation {
  pname = "fm-assistant";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory exceptions foldl managed mtl streaming
    streaming-bytestring system-filepath tar temporary text
    transformers turtle
  ];
  executableHaskellDepends = [
    base bytestring directory exceptions foldl managed mtl
    optparse-applicative streaming streaming-bytestring system-filepath
    tar temporary text transformers turtle
  ];
  testHaskellDepends = [
    base bytestring directory doctest exceptions foldl hspec managed
    mtl streaming streaming-bytestring system-filepath tar temporary
    text transformers turtle
  ];
  license = stdenv.lib.licenses.bsd3;
}
