(import ../../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    hello-server = ./hello-server;
  };

  shells = {
    ghc = ["hello-server"];
  };

  tools = ghc: [ ghc.ghcid ];
})
