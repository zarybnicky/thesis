# A skeleton of a Progressive Web Application

## Commands
To get a development server, you can run `ghcid-here project`, which will run a `ghcid`
runner of a `cabal new-repl`. This entrypoint will use the Main module in
`src-bin/main-warp.hs`, making the Reflex application, the service worker, the
WebManifest, and any static assets from `static/` available at
http://localhost:3000.

To build a production-ready GHCJS bundle, run `nix-build -A ghcjs.project` in
this directory, which will compile and run `src-bin/main-ghcjs.hs` and
`src-bin/main-gen.hs`, generating a JavaScript bundle, a service worker and a
WebManifest, combined with any static assets.

This bundle however needs to be served from a web server and not from the file
system, so you will need a simple file server like `npx http-server`,
`php -S localhost:3000`, or `python3 -m http.server` to serve the files.
