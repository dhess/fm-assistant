{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , doctest, exceptions, filepath, foldl, hspec, lens, mtl
      , optparse-applicative, process-streaming, resourcet, stdenv
      , streaming, streaming-bytestring, system-filepath, tar
      , template-haskell, temporary, text, transformers
      }:
      mkDerivation {
        pname = "fm-assistant";
        version = "0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring containers directory exceptions filepath foldl lens
          mtl process-streaming resourcet streaming streaming-bytestring
          system-filepath tar template-haskell temporary text transformers
        ];
        executableHaskellDepends = [
          base bytestring containers directory exceptions filepath foldl lens
          mtl optparse-applicative process-streaming resourcet streaming
          streaming-bytestring system-filepath tar template-haskell temporary
          text transformers
        ];
        testHaskellDepends = [
          base bytestring containers directory doctest exceptions filepath
          foldl hspec lens mtl process-streaming resourcet streaming
          streaming-bytestring system-filepath tar template-haskell temporary
          text transformers
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
