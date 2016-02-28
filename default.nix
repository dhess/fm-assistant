{ mkDerivation, base, hspec, optparse-applicative, stdenv }:
mkDerivation {
  pname = "fm-assistant";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base optparse-applicative ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
