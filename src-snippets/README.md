# Snippets

In this part of the repository, you'll find short snippets of code showing
component pieces of functionality. While these are intended more as a source of
ready-made code and a playground for me rather than a formal demonstration, I'll
try to structure it into a walkthrough of the concepts required for the framework.

## Prerequisites
I assume that the host system is NixOS in all examples, which also means that
all NixOps examples use the built-in NixOS containers (a wrapper around systemd
containers). There are several ways to get NixOS up and running on your
computer, here are a few of them:

### Docker
If you have Docker installed on your machine, I've prepared a Docker image
prebuilt with all the necessary tools and dependencies ready to go without
fetching anything else from the internet.

TODO:

### VirtualBox
If you have VirtualBox installed on your machine, I've prepared an `.ova`
prebuilt with all the necessary tools and dependencies ready to go without
fetching anything else from the internet.

TODO:

### Nix, NixOS and NixOps
If you're already using NixOS, simply entering `nix-shell` in each snippet's
directory will give you all the tools you need, including NixOps and others.


## Introducing the technologies
- [A simple Haskell server (Servant)](https://github.com/zarybnicky/thesis/tree/master/src-snippets/simple-server)
- [A simple Haskell client (Reflex)](https://github.com/zarybnicky/thesis/tree/master/src-snippets/simple-client)
- [A simple Service Worker application (plain JavaScript)](https://github.com/zarybnicky/thesis/tree/master/src-snippets/offline-available-manual)

## Deployment
- [Deployment of a Haskell server to NixOps](https://github.com/zarybnicky/thesis/tree/master/src-snippets/deployment-nixops)
- Continuous deployment using Hail

## Quality assurance
- Unit tests of ? (using hedgehog, tasty?)
- Integration tests of a server
- Integration tests of a client
- End-to-end tests of a server and client together
- Simple benchmarks (criterion)
- Allocation benchmarks (weigh)
- Measuring the evolution of performance of a library
- Continuous integration using Hydra

## Tools
- A scaffolding tool
- Remote debugging console (CloudHaskell)

## Techniques
- Prebuilt frontend files
- Build-on-demand frontend
