(import ../../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    service-listener = ./service-listener;
  };

  shells = {
    ghc = ["service-listener"];
    ghcjs = ["service-listener"];
  };

  tools = ghc: [ ghc.ghcid ];

  overrides = with pkgs.haskell.lib; self: super: {
    service-listener = overrideCabal super.service-listener (drv: {
      postFixup = ''
        if [ -n $out/bin/service-listener.jsexe ]; then
          echo "GHC only build? Not compressing JS"
          exit 0
        fi
        pushd $out/bin/service-listener.jsexe
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
        cp ${./static}/* .
        popd
      '';
    });
  };
})
