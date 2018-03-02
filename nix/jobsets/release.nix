let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ (import ../../.) ];
  }
}:

with import (fixedNixPkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  jobs = {

    nixpkgs = pkgs.releaseTools.aggregate {
      name = "nixpkgs";
      meta.description = "fm-assistant built against nixpkgs haskellPackages";
      meta.maintainer = lib.maintainers.dhess-pers;
      constituents = with jobs; [
        haskellPackages.fm-assistant.x86_64-darwin
        haskellPackages.fm-assistant.x86_64-linux
      ];
    };

  } // (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  inherit (jobs) nixpkgs;
}
