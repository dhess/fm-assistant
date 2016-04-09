{ mkDerivation, base, bytestring, cond, containers, directory
, doctest, exceptions, filepath, foldl, hspec, lens, lzma, mtl
, optparse-applicative, path, path-io, process-streaming, resourcet
, stdenv, streaming, streaming-bytestring, system-filepath, tar
, template-haskell, temporary, text, time, transformers
}:
mkDerivation {
  pname = "fm-assistant";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cond containers directory exceptions filepath foldl
    lens lzma mtl path path-io process-streaming resourcet streaming
    streaming-bytestring system-filepath tar template-haskell temporary
    text time transformers
  ];
  executableHaskellDepends = [
    base bytestring cond containers directory exceptions filepath foldl
    lens lzma mtl optparse-applicative path path-io process-streaming
    resourcet streaming streaming-bytestring system-filepath tar
    template-haskell temporary text time transformers
  ];
  testHaskellDepends = [
    base bytestring cond containers directory doctest exceptions
    filepath foldl hspec lens lzma mtl path path-io process-streaming
    resourcet streaming streaming-bytestring system-filepath tar
    template-haskell temporary text time transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
