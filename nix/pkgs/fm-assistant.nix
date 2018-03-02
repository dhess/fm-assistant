{ mkDerivation, base, bytestring, cond, containers, directory
, doctest, exceptions, filepath, foldl, hlint, hspec, lens, lzma
, mtl, optparse-applicative, path, path-io, process-streaming
, resourcet, stdenv, streaming, streaming-bytestring
, system-filepath, tar, template-haskell, temporary, text, time
, transformers
}:
mkDerivation {
  pname = "fm-assistant";
  version = "0.6.0.0";
  src = ../../.;
  configureFlags = [ "-ftest-hlint" ];
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring cond containers directory exceptions filepath foldl
    lens lzma mtl path path-io process-streaming resourcet streaming
    streaming-bytestring system-filepath tar template-haskell temporary
    text time transformers
  ];
  executableHaskellDepends = [
    base exceptions optparse-applicative path path-io
  ];
  testHaskellDepends = [
    base doctest hlint hspec path path-io resourcet time
  ];
  homepage = "https://github.com/dhess/fm-assistant#readme";
  description = "Install Football Manager mods";
  license = stdenv.lib.licenses.bsd3;
}
