self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalFMAssistant;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

  fmAssistantPath = ../pkgs/fm-assistant.nix;

in
{

  haskellPackages =
    withLocalFMAssistant fmAssistantPath (super.haskellPackages.extend (self: super:
      rec {
        lzma = doJailbreak super.lzma;
        pipes-transduce = dontCheck super.pipes-transduce;
      }
  ));

}
