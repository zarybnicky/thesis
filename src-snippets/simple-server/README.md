# Trivial Servant server

Servant is a Haskell server-side routing library. This is a simple Servant
server with three endpoints that show Servant's basic features.

This is meant less as a demonstration and more as a starting point for me when
writing the other snippets.

TODO: Add Post, SetCookie, ...

*Requirements*: Nix

```
nix-build
result/bin/trivial-server
# Now listening on port 3000
```

The server's API definition is the following:

```haskell
type Api =
  Get '[PlainText] Text :<|>
  "hello" :> Capture "name" Text :> Get '[JSON] [Text]
```

This defines two endpoints (separated by `:<|>`). One is the root handler that
doesn't take any inputs and returns a plain-text swagger output (actually a
pretty-printed JSON, oh well...), and the other taes one parameter (a path
component) and returns a JSON value containing a list of strings.

The implementation is also pretty simple:

```haskell
server :: Server Api
server = swagger :<|> hello

swagger :: Handler Text
swagger = return (pack (unpack (encodePretty (toSwagger (Proxy @Api)))))

hello :: Text -> Handler [Text]
hello x = return ["Hello", x, "!"]
```
