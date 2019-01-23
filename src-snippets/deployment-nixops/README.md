# Deploying a Haskell server to NixOps

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
