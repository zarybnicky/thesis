(import ../../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    trivial-client = ./.;
  };

  shells = {
    ghc = ["trivial-client"];
    ghcjs = ["trivial-client"];
  };

  tools = ghc: [ ghc.ghcid ];
})
