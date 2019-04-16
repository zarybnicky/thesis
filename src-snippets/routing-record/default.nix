(import ../../reflex-platform {}).project ({ pkgs, ghc, ... }: rec {
  packages = {
    routing-record = ./.;
  };

  shells = let p = builtins.attrNames packages; in { ghc = p; ghcjs = p; };
  tools = ghc: [ ghc.ghcid ];
  overrides = with pkgs.haskell.lib; self: super: {

  };
})
