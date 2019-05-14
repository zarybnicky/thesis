(import ../../reflex-platform {}).project ({ pkgs, ghc, ... }: rec {
  packages = {
    service-worker = ./.;
  };

  shells = let p = builtins.attrNames packages; in { ghc = p; ghcjs = p; };
  tools = _: [ ghc.ghcid ghc.happy ];
  overrides = with pkgs.haskell.lib; self: super: {
    parseargs = dontCheck super.parseargs;
  };
})
