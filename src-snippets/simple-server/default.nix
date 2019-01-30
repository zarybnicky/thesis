{ nixpkgs ? (import ../../reflex-platform {}).nixpkgs }:
nixpkgs.pkgs.haskellPackages.callPackage ./trivial-server.nix { }
