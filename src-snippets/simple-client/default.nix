(import ../../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    simple-client = ./.;
  };

  shells = {
    ghc = ["simple-client"];
    ghcjs = ["simple-client"];
  };

  tools = ghc: [ ghc.ghcid ];
})
