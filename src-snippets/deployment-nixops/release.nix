{ reflex-platform ? (import ../../reflex-platform {})
, nixpkgs ? reflex-platform.nixpkgs
, nixops ? nixpkgs.pkgs.nixops
}:

let
  lib = nixpkgs.pkgs.lib;

  inherit (import "${nixops}/share/nix/nixops/eval-machine-info.nix" {
    networkExprs = [ ./network.nix ];
    uuid = "dummy";
    deploymentName = "dummy";
    args = [];
  }) nodes;

in lib.mapAttrs' (n: v: lib.nameValuePair ("deployment-nixops-" + n) (v.config.system.build.toplevel)) nodes
