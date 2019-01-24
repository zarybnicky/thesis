(import ../../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    hello-app = ./hello-app;
  };

  shells = {
    ghc = ["hello-app"];
    ghcjs = ["hello-app"];
  };

  tools = ghc: [ ghc.ghcid ];

  overrides = with pkgs.haskell.lib; self: super: {
    hello-app = overrideCabal super.hello-app (drv: {
      postFixup = ''
        pushd $out/bin/hello-app.jsexe
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
