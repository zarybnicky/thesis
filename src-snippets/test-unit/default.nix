{ nixpkgs ? (import ../../reflex-platform {}).nixpkgs }:
nixpkgs.pkgs.haskellPackages.callPackage ./test-unit.nix { }
