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
        pushd $out/bin/service-listener.jsexe
        ${pkgs.closurecompiler}/bin/closure-compiler all.js \
          -O ADVANCED \
          --externs=all.js.externs \
          --jscomp_off=checkVars \
          > ../all.min.js
        popd
      '';
    });
  };
})
