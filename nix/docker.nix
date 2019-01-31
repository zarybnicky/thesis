{ reflex-platform ? (import ../reflex-platform {})
, nixpkgs ? reflex-platform.nixpkgs
, pkgs ? nixpkgs.pkgs
}:

with pkgs;

let
  eval = (import "${nixpkgs.path}/nixos/lib/eval-config.nix" {
    modules = [{
      imports = [
        "${nixpkgs.path}/nixos/modules/profiles/docker-container.nix"
      ];
      environment.systemPackages = [ nixpkgs.pkgs.nixops ];
    }];
  });

in dockerTools.buildImageWithNixDb {
  name = "zarybnicky/nixos-thesis";
  tag = "latest";
  created = "now";
  contents = [
    eval.config.system.build.etc
    eval.config.system.path
  ] ++ builtins.attrValues (import ../src-snippets);
  runAsRoot = ''
    #!${stdenv.shell}
    export PATH=/bin:/usr/bin:/sbin:/usr/sbin:$PATH
    ${dockerTools.shadowSetup}
    mkdir -m 777 tmp
  '';
  config = {
    EntryPoint = [ "bash" ];
  };
}
