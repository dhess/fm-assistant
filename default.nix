{ mkDerivation, base, bytestring, containers, directory, doctest
, exceptions, foldl, hspec, managed, mtl, optparse-applicative
, resourcet, stdenv, streaming, streaming-bytestring
, system-filepath, tar, temporary, text, transformers, turtle
}:
mkDerivation {
  pname = "fm-assistant";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory exceptions foldl managed mtl
    resourcet streaming streaming-bytestring system-filepath tar
    temporary text transformers turtle
  ];
  executableHaskellDepends = [
    base bytestring containers directory exceptions foldl managed mtl
    optparse-applicative resourcet streaming streaming-bytestring
    system-filepath tar temporary text transformers turtle
  ];
  testHaskellDepends = [
    base bytestring containers directory doctest exceptions foldl hspec
    managed mtl resourcet streaming streaming-bytestring
    system-filepath tar temporary text transformers turtle
  ];
  license = stdenv.lib.licenses.bsd3;
}
