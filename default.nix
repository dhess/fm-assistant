{ mkDerivation, base, bytestring, containers, directory, doctest
, exceptions, filepath, foldl, hspec, mtl, optparse-applicative
, process-streaming, resourcet, stdenv, streaming
, streaming-bytestring, system-filepath, tar, temporary, text
, transformers
}:
mkDerivation {
  pname = "fm-assistant";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory exceptions filepath foldl mtl
    process-streaming resourcet streaming streaming-bytestring
    system-filepath tar temporary text transformers
  ];
  executableHaskellDepends = [
    base bytestring containers directory exceptions filepath foldl mtl
    optparse-applicative process-streaming resourcet streaming
    streaming-bytestring system-filepath tar temporary text
    transformers
  ];
  testHaskellDepends = [
    base bytestring containers directory doctest exceptions filepath
    foldl hspec mtl process-streaming resourcet streaming
    streaming-bytestring system-filepath tar temporary text
    transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
