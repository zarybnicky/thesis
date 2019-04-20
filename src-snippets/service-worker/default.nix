(import ../../reflex-platform {}).project ({ pkgs, ghc, ... }: rec {
  packages = {
    service-worker = ./.;
    domconv-webkit = ./domconv-webkit;
    WebBits = ./webbits;
    jmacro = ./jmacro;
  };

  shells = let p = builtins.attrNames packages; in { ghc = p; ghcjs = p; };
  tools = _: [ ghc.ghcid ghc.happy ];
  overrides = with pkgs.haskell.lib; self: super: {
    parseargs = dontCheck super.parseargs;
  };
})
