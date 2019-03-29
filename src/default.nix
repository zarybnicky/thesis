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
    extra = dontCheck (super.extra);

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
