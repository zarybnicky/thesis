(import ../../reflex-platform {}).project ({ pkgs, ... }: rec {
  packages = {
    storage-trans = ./.;
  };

  shells = let p = builtins.attrNames packages; in { ghc = p; ghcjs = p; };
  tools = ghc: [ ghc.ghcid ];
  overrides = with pkgs.haskell.lib; self: super: {

  };
})
