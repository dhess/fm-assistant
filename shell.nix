{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cond, containers, directory
      , doctest, exceptions, filepath, foldl, hspec, lens, lzma, mtl
      , optparse-applicative, path, path-io, process-streaming, resourcet
      , stdenv, streaming, streaming-bytestring, system-filepath, tar
      , template-haskell, temporary, text, time, transformers
      }:
      mkDerivation {
        pname = "fm-assistant";
        version = "0.5.2";
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        lzma = pkgs.haskell.lib.doJailbreak super.lzma;
      };
  };
  drv = modifiedHaskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
