(import ../reflex-platform {}).project ({ pkgs, ghc, ... }: rec {
  packages = {
    tapaw-7guis = ./tapaw-7guis;
    tapaw-todomvc = ./tapaw-todomvc;
    tapaw-hnpwa = ./tapaw-hnpwa;
    tapaw-realworld-client = ./tapaw-realworld-client;
  };

  shells = {
    ghc = builtins.attrNames packages;
    ghcjs = builtins.attrNames packages;
  };

  tools = _: [
    ghc.ghcid
    (import ../src-snippets/ghcid-here { inherit ghc pkgs; })
  ];

  overrides = with pkgs.haskell.lib; self: super: {
    inspection-testing = if self.ghc.isGhcjs or false then null else super.inspection-testing;
    mockery = if self.ghc.isGhcjs or false then null else super.mockery;
    generic-lens = (if self.ghc.isGhcjs or false then dontCheck else (x: x)) super.generic-lens;
    extra = (if self.ghc.isGhcjs or false then dontCheck else (x: x)) super.extra;
    servant = (if self.ghc.isGhcjs or false then dontCheck else (x: x)) super.servant;
    http-date = (if self.ghc.isGhcjs or false then dontCheck else (x: x)) super.http-date;
    jsaddle-warp = if self.ghc.isGhcjs or false then self.callHackage "jsaddle-warp" "0.9.6.0" {} else super.jsaddle-warp;
    parseargs = dontCheck super.parseargs;
    tapaw-route = super.callCabal2nix "tapaw-route" ../src/tapaw-route {};
    tapaw-serviceworker = super.callCabal2nix "tapaw-serviceworker" ../src/tapaw-serviceworker {};
    tapaw-storage = super.callCabal2nix "tapaw-storage" ../src/tapaw-storage {};
    tapaw-webmanifest = super.callCabal2nix "tapaw-webmanifest" ../src/tapaw-webmanifest {};

    servant-reflex = doJailbreak super.servant-reflex;
    polysemy = (if self.ghc.isGhcjs or false then dontCheck else (x: x)) (super.callCabal2nix "polysemy" (pkgs.fetchFromGitHub {
      owner = "isovector";
      repo = "polysemy";
      rev = "fbbed8d2c682df201c86132467694b8827022f35";
      sha256 = "0p66d7r0v2s2wkpc1nsv7pg1arpsdqj0a26y730bmlnas3flyn8b";
    }) { hpack = ghc.hpack; });

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
        rm manifest.webapp
        ${pkgs.nodejs}/bin/node $out/bin/gen.jsexe/all.js
        cp ${./tapaw-hnpwa/static}/* .
        popd
      '';
    });

    tapaw-realworld-client = overrideCabal super.tapaw-realworld-client (drv: {
      postFixup = ''
        if [ ! -e $out/bin/tapaw-realworld-client.jsexe ]; then
          echo "GHC only build? Not compressing JS"
          exit 0
        fi
        pushd $out/bin/tapaw-realworld-client.jsexe
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
        rm manifest.webapp
        ${pkgs.nodejs}/bin/node $out/bin/gen.jsexe/all.js
        cp -r ${./tapaw-realworld-client/static}/* .
        popd
      '';
    });
  };
})
