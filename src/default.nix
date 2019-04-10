(import ../reflex-platform {}).project ({ pkgs, ghc8_4, ... }: {
  packages = {
    tapaw-core = ./tapaw-core;
    tapaw-7guis = ./tapaw-7guis;
    tapaw-todomvc = ./tapaw-todomvc;
    tapaw-hnpwa = ./tapaw-hnpwa;
    tapaw-realworld-client = ./tapaw-realworld-client;
  };

  shells = {
    ghc = ["tapaw-core" "tapaw-7guis" "tapaw-todomvc" "tapaw-hnpwa" "tapaw-realworld-client"];
    ghcjs = ["tapaw-core" "tapaw-7guis" "tapaw-todomvc" "tapaw-hnpwa" "tapaw-realworld-client"];
  };

  tools = ghc: [
    ghc8_4.ghcid
    (import ../src-snippets/ghcid-here { inherit pkgs; ghc = ghc8_4; })
  ];

  overrides = with pkgs.haskell.lib; self: super: {
    inspection-testing = if self.ghc.isGhcjs or false then null else super.inspection-testing;
    generic-lens = dontCheck super.generic-lens;
    hpack = dontCheck super.hpack;
    extra = dontCheck super.extra;
    Glob = dontCheck super.Glob;
    servant-reflex = doJailbreak super.servant-reflex;
    polysemy = super.callCabal2nix "polysemy" (pkgs.fetchFromGitHub {
      owner = "isovector";
      repo = "polysemy";
      rev = "fbbed8d2c682df201c86132467694b8827022f35";
      sha256 = "0p66d7r0v2s2wkpc1nsv7pg1arpsdqj0a26y730bmlnas3flyn8b";
    }) {};

    tapaw-hnpwa = overrideCabal super.tapaw-hnpwa (drv: {
      postFixup = ''
        if [ ! -e $out/bin/tapaw-hnpwa.jsexe ]; then
          echo "GHC only build? Not compressing JS"
          exit 0
        fi
        pushd $out/bin/tapaw-hnpwa.jsexe
        mv all.js all.unminified.js
        ${pkgs.closurecompiler}/bin/closure-compiler \
          all.unminified.js \
          -O ADVANCED \
          --externs=all.js.externs \
          --jscomp_off=checkVars \
          --create_source_map="all.js.map" \
          --source_map_format=V3 \
          > all.js
        echo "//# sourceMappingURL=all.js.map" >> all.js
        cp ${./tapaw-hnpwa/static}/* .
        popd
      '';
    });
  };
})
