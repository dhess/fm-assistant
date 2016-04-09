{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cond, containers, directory
      , doctest, exceptions, filepath, foldl, hspec, lens, mtl
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
          lens mtl path path-io process-streaming resourcet streaming
          streaming-bytestring system-filepath tar template-haskell temporary
          text time transformers
        ];
        executableHaskellDepends = [
          base bytestring cond containers directory exceptions filepath foldl
          lens mtl optparse-applicative path path-io process-streaming
          resourcet streaming streaming-bytestring system-filepath tar
          template-haskell temporary text time transformers
        ];
        testHaskellDepends = [
          base bytestring cond containers directory doctest exceptions
          filepath foldl hspec lens mtl path path-io process-streaming
          resourcet streaming streaming-bytestring system-filepath tar
          template-haskell temporary text time transformers
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
