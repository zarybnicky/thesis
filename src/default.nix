(import ../reflex-platform {}).project ({ pkgs, ghc, ... }: rec {
  packages = {
    tapaw-route = ./tapaw-route;
    tapaw-serviceworker = ./tapaw-serviceworker;
    tapaw-storage = ./tapaw-storage;
    tapaw-storage-jsaddle = ./tapaw-storage-jsaddle;
    tapaw-storage-persistent = ./tapaw-storage-persistent;
    tapaw-webmanifest = ./tapaw-webmanifest;
  };

  shells = let p = builtins.attrNames packages; in { ghc = p; ghcjs = p; };
  tools = _: [
    ghc.ghcid
    (import ../src-snippets/ghcid-here { inherit ghc pkgs; })
  ];
  overrides = with pkgs.haskell.lib; self: super: {
    parseargs = dontCheck super.parseargs;
    jsaddle-warp = if self.ghc.isGhcjs or false then self.callHackage "jsaddle-warp" "0.9.6.0" {} else super.jsaddle-warp;
  };
})
