(import ../../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    app-7guis = ./.;
  };

  shells = {
    ghc = ["app-7guis"];
    ghcjs = ["app-7guis"];
  };

  tools = ghc: [ ghc.ghcid ];
})
