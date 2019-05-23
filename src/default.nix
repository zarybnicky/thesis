(import ../reflex-platform {}).project ({ pkgs, ghc, ... }: rec {
  packages = {
    tapaw-route = ./tapaw-route;
    tapaw-serviceworker = ./tapaw-serviceworker;
    tapaw-storage = ./tapaw-storage;
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
    web-push = self.callCabal2nix "web-push" (pkgs.fetchFromGitHub {
      owner = "sarthakbagaria";
      repo = "web-push";
      rev = "43f9bb7b23377840d3ee5e7d1e93c0fcee32d859";
      sha256 = "0p66d7r0v2s2wkpc1nsv7pg1arpsdqj0a26y730bmlnas3flyn8a";
    });
  };
})
