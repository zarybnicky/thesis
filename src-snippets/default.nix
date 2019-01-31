let
  simple-client = import ./simple-client;
  simple-server = import ./simple-server;
  offline-available-manual = import ./offline-available-manual;
  deployment-nixops = import ./deployment-nixops;
  deployment-nixops-release = import ./deployment-nixops/release.nix;
in
{
  inherit (deployment-nixops-release {});
  simple-server = simple-server {};
  simple-client-ghcjs = simple-client.ghcjs.simple-client;
  simple-client-ghc = simple-client.ghc.simple-client;
  offline-available-manual-ghcjs = offline-available-manual.ghcjs.service-listener;
  offline-available-manual-ghc = offline-available-manual.ghc.service-listener;
}
