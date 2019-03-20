(import ../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    tapaw-core = ./tapaw-core;
    tapaw-7guis = ./tapaw-7guis;
    tapaw-todomvc = ./tapaw-todomvc;
    tapaw-hnpwa = ./tapaw-hnpwa;
  };

  shells = {
    ghc = ["tapaw-core" "tapaw-7guis" "tapaw-todomvc" "tapaw-hnpwa"];
    ghcjs = ["tapaw-core" "tapaw-7guis" "tapaw-todomvc" "tapaw-hnpwa"];
  };

  tools = ghc: [
    ghc.ghcid
    (import ../src-snippets/ghcid-here { inherit pkgs ghc; })
  ];

  overrides = with pkgs.haskell.lib; self: super: {
    generic-lens = dontCheck (super.generic-lens);
  };
})
