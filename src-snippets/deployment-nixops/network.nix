{
  webserver = { pkgs, ... }: {
    deployment.targetEnv = "container";

    require = [ ./module.nix ];

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    services.helloServer.enable = true;
    services.helloServer.enableReverseProxy = true;
  };
}
