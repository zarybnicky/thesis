# Trivial Reflex client

Reflex is a GUI library capable of running in the browser (among other
environments, like Android/iOS or desktop). This snippet contains a basic
example of what such an application looks like.

*Requirements*: Nix

For building the application to JavaScript:
```
nix-build -A ghcjs.trivial-client
xdg-open result/bin/trivial-client.jsexe/index.html
```
