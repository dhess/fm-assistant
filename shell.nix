{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, directory, doctest
      , exceptions, hspec, managed, mtl, optparse-applicative, stdenv
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
          base bytestring directory exceptions managed mtl streaming
          streaming-bytestring system-filepath tar temporary text
          transformers turtle
        ];
        executableHaskellDepends = [
          base bytestring directory exceptions managed mtl
          optparse-applicative streaming streaming-bytestring system-filepath
          tar temporary text transformers turtle
        ];
        testHaskellDepends = [
          base bytestring directory doctest exceptions hspec managed mtl
          streaming streaming-bytestring system-filepath tar temporary text
          transformers turtle
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
