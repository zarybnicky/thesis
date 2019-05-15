(import ../../reflex-platform {}).project ({ pkgs, ghc, ... }: rec {
  packages = {
    project = ./.;
  };

  shells = {
    ghc = builtins.attrNames packages;
    ghcjs = builtins.attrNames packages;
  };

  tools = _: [
    ghc.ghcid
    (import ../ghcid-here { inherit ghc pkgs; })
  ];

  overrides = with pkgs.haskell.lib; self: super: {
    parseargs = dontCheck super.parseargs;
    tapaw-serviceworker = super.callCabal2nix "tapaw-serviceworker" ../../src/tapaw-serviceworker {};
    tapaw-webmanifest = super.callCabal2nix "tapaw-webmanifest" ../../src/tapaw-webmanifest {};
    jsaddle-warp = if self.ghc.isGhcjs or false then self.callHackage "jsaddle-warp" "0.9.6.0" {} else super.jsaddle-warp;

    project = overrideCabal super.project (drv: {
      postFixup = ''
        if [ ! -e $out/bin/project.jsexe ]; then
          echo "GHC only build? Not compressing JS"
          exit 0
        fi
        pushd $out/bin/project.jsexe
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
        ${pkgs.nodejs}/bin/node $out/bin/project-gen.jsexe/all.js
        cp -r ${./static}/* .
        popd
      '';
    });
  };
})
