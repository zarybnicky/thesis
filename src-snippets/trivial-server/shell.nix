{ nixpkgs ? (import ../../reflex-platform {}).nixpkgs }:
let
  inherit (nixpkgs) pkgs;
  drv = pkgs.haskellPackages.callPackage ./. {};
in
  if pkgs.lib.inNixShell then drv.env else drv
