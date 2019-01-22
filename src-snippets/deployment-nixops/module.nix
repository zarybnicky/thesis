{ config, pkgs, lib, ... }:

with lib;

{
  options = {
    services.helloServer = {
      enable = mkEnableOption "Hello Server";

      enableReverseProxy = mkEnableOption "Hello Server - reverse proxy";

      package = mkOption {
        default = (import ./.).ghc.hello-server;
        defaultText = "pkgs.minio";
        type = types.package;
        description = "Package to use.";
      };
    };
  };

  config = mkIf cfg.enable {
    users = {
      groups = {
        helloserver = {
          gid = 400;
        };
      };
      users = {
        helloserver = {
          group = "users";
          uid = 400;
        };
      };
    };

    systemd.services.nginx.virtualHosts.default = mkIf config.enableReverseProxy {
      locations."/" = {
        proxyPass = "http://localhost:3000";
      };
    };

    systemd.services.hello-server = {
      description = "Hello Server";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        User = "helloserver";
        Group = "helloserver";
        ExecStart = "${config.package}/bin/hello-server";
      };
    };
  };
}
