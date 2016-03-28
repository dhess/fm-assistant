{ mkDerivation, base, bytestring, cond, containers, directory
, doctest, exceptions, filepath, foldl, hspec, lens, mtl
, optparse-applicative, path, path-io, process-streaming, resourcet
, stdenv, streaming, streaming-bytestring, system-filepath, tar
, template-haskell, temporary, text, transformers
}:
mkDerivation {
  pname = "fm-assistant";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cond containers directory exceptions filepath foldl
    lens mtl path path-io process-streaming resourcet streaming
    streaming-bytestring system-filepath tar template-haskell temporary
    text transformers
  ];
  executableHaskellDepends = [
    base bytestring cond containers directory exceptions filepath foldl
    lens mtl optparse-applicative path path-io process-streaming
    resourcet streaming streaming-bytestring system-filepath tar
    template-haskell temporary text transformers
  ];
  testHaskellDepends = [
    base bytestring cond containers directory doctest exceptions
    filepath foldl hspec lens mtl path path-io process-streaming
    resourcet streaming streaming-bytestring system-filepath tar
    template-haskell temporary text transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
