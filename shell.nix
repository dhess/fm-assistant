{ compiler ? "ghc822"
, overlays ? [ (import ./.) ]
}:

let

  fixedNixPkgs = (import ./nix/lib.nix).fetchNixPkgs;

  pkgs = (import fixedNixPkgs) { inherit overlays; };

  drv = pkgs.haskellPackages.fm-assistant;

in

  if pkgs.lib.inNixShell then drv.env else drv
