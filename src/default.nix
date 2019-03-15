(import ../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    tapaw-core = ./tapaw-core;
    tapaw-7guis = ./tapaw-7guis;
  };

  shells = {
    ghc = ["tapaw-core" "tapaw-7guis"];
    ghcjs = ["tapaw-core" "tapaw-7guis"];
  };

  tools = ghc: [ ghc.ghcid ];

  overrides = with pkgs.haskell.lib; self: super: {
    generic-lens = dontCheck (super.generic-lens);
  };
})
