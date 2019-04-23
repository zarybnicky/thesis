(import ../../reflex-platform {}).project ({ pkgs, ghc, ... }: rec {
  packages = {
    effect-base = ./.;
  };

  shells = let p = builtins.attrNames packages; in { ghc = p; ghcjs = p; };
  tools = ghc: [ ghc.ghcid ];
  overrides = with pkgs.haskell.lib; self: super: {
    polysemy = (if self.ghc.isGhcjs or false then dontCheck else (x: x))
    (disableCabalFlag (super.callCabal2nix "polysemy" (pkgs.fetchFromGitHub {
      owner = "isovector";
      repo = "polysemy";
      rev = "fbbed8d2c682df201c86132467694b8827022f35";
      sha256 = "0p66d7r0v2s2wkpc1nsv7pg1arpsdqj0a26y730bmlnas3flyn8b";
    }) { hpack = ghc.hpack; }) "error-messages");
  };
})
