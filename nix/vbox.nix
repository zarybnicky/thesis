{ reflex-platform ? (import ../reflex-platform {})
, nixpkgs ? reflex-platform.nixpkgs
, pkgs ? nixpkgs.pkgs
}:

let
  baseSystem = import "${nixpkgs.path}/nixos/lib/eval-config.nix" {
    modules = [{
      imports = [ "${nixpkgs.path}/nixos/modules/virtualisation/virtualbox-image.nix" ];
      nixpkgs.system = "x86_64-linux";
      services.xserver.videoDrivers = [ "virtualbox" "vmware" "cirrus" "vesa" "modesetting" ];
      powerManagement.enable = false;
      nix.trustedUsers = [ "demo" ];
      users.users.demo = {
        isNormalUser = true;
        description = "Demo user account";
        extraGroups = [ "wheel" "vboxsf" ];
        password = "demo";
        uid = 1000;
      };
      services.openssh.enable = true;
      boot.vesa = false;
      boot.loader.timeout = 1;
      boot.initrd = {
        kernelModules = [ "vboxsf" ];
        extraUtilsCommands = ''
          cp -v ${pkgs.linuxPackages.virtualboxGuestAdditions}/bin/mount.vboxsf $out/bin/
        '';
      };
      systemd.services.dhcpcd.restartIfChanged = false;

      systemd.services.copy-app-files = {
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
        };
        script = ''
          mkdir -p /home/demo/thesis
          cp -r ${../.}/* /home/demo/thesis/
        '';
      };
      environment.systemPackages = [
        pkgs.direnv
        pkgs.nixops
      ] ++ builtins.attrValues (import ../src-snippets);
    }];
  };
  ova = baseSystem.config.system.build.virtualBoxOVA;
  vmdk = pkgs.runCommand "nixops-ova-to-vmdk" {} ''
    mkdir -p $out
    echo ${ova}
    tar -xf ${ova}/*.ova && mv nixos*.vmdk $out/nixos.vmdk
  '';
in vmdk