# Deploying a Haskell server to NixOps

This snippets illustrates a trivial deployment of a Servant server to NixOps. We
use reflex-platform (as with the other projects) to build the executable
(`default.nix`), create a NixOS module that creates a systemd service for the
server that optionally adds a Nginx reverse proxy (`module.nix`). `network.nix`
wraps this up - we specify a NixOps deployment there, where we deploy to a
VirtualBox machine, enable ports 80 and 443 in its firewall, and start the
service and reverse proxy.

*Prerequisites*: direnv, nixops, VirtualBox

```
direnv allow
nixops create network.nix
nixops deploy
```

Afterwards, `nixops info` will tell you the IP address of the deployed machine,
where you can access the server.

API:
```
type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item
```
