{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, exceptions, hspec, mtl
      , optparse-applicative, stdenv, streaming, streaming-bytestring
      , tar, temporary, transformers
      }:
      mkDerivation {
        pname = "fm-assistant";
        version = "0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring exceptions mtl streaming streaming-bytestring tar
          temporary transformers
        ];
        executableHaskellDepends = [
          base bytestring exceptions mtl optparse-applicative streaming
          streaming-bytestring tar temporary transformers
        ];
        testHaskellDepends = [
          base bytestring exceptions hspec mtl streaming streaming-bytestring
          tar temporary transformers
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
