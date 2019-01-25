{ nixpkgs ? (import ../../reflex-platform {}).nixpkgs }:
(nixpkgs.pkgs.haskellPackages.callPackage ./. {}).env
