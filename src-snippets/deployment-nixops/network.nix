{
  webserver = { pkgs, ... }: {
    deployment.targetEnv = "virtualbox";

    deployment.virtualbox.headless = true;
    deployment.virtualbox.disks.disk1.baseImage = let
      baseSystem = (import <nixpkgs/nixos> {
        configuration = "${pkgs.nixops}/share/nix/nixops/virtualbox-image-nixops.nix";
      });
      version = baseSystem.config.system.nixos.version;
      ova = baseSystem.config.system.build.virtualBoxOVA;
      vmdk = pkgs.runCommand "nixops-ova-to-vmdk" {} ''
        mkdir -p $out
        echo ${ova}
        tar -xf ${ova}/*.ova && mv nixos*.vmdk $out/nixos.vmdk
      '';
    in "${vmdk}/nixos.vmdk";

    require = [ ./module.nix ];

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    services.helloServer.enable = true;
    services.helloServer.enableReverseProxy = true;
  };
}
