{ reflex-platform ? (import ../reflex-platform {})
, nixpkgs ? reflex-platform.nixpkgs
, pkgs ? nixpkgs.pkgs
}:

with pkgs;

let
  system = (import "${nixpkgs.path}/nixos/lib/eval-config.nix" {
    modules = [{
      imports = [
        "${nixpkgs.path}/nixos/modules/profiles/docker-container.nix"
      ];
      environment.systemPackages = [ nixpkgs.pkgs.nixops ];
    }];
  }).config.system;

in dockerTools.buildImageWithNixDb {
  name = "zarybnicky/nixos-thesis";
  tag = "latest";
  created = "now";
  contents = [
    system.build.etc
    system.path
  ] ++ builtins.attrValues (import ../src-snippets);
  config = {
    Cmd = ["/bin/bash"];
    Env = ["LANG=en_US.UTF-8"];
  };
}
