# A Haskell framework for Progressive Web Applications
This repository contains all code and documentation related to my bachelor
thesis:

ZÁRYBNICKÝ, Jakub. A Haskell Platform for Creating Progressive Web
Applications. Brno, 2019. Backelor's thesis. Brno University of Technology,
Faculty of Information Technology. Supervisor Ing. Ondřej Lengál, Ph.D.

## Rendered PDFs
- [Midterm progress report](doc-midterm-report/midterm-report.pdf)
- [Midterm progress presentation](doc-midterm-presentation/midterm-presentation.pdf)
- [Thesis](doc-final-thesis/projekt.pdf)

## Tools
For trying out the code in this repository, you either need to install
[Nix](https://nixos.org/nix/) and [NixOps](https://nixos.org/nixops/) using the
instructions on their websites, or use a VirtualBox appliance with all the
necessary tools and dependencies - get it
[here](https://zarybnicky.com/static/nixos-18.09pre-git-x86_64-linux.ova). The
login is `demo` (password `demo`), and the contents of this repository are
bundled with the image in `~/thesis`. The source code for the image is available
in [nix/vbox.nix](nix/vbox.nix)).

## Repository structure
- `doc-midterm-report/` - midterm progress report
- `doc-midterm-presentation/` - presentation for the midterm defence of the thesis
- `doc-final-thesis/` - sources for the thesis itself
- ~~`doc-final-presentation/` - presentation for the final defence of the thesis~~
- `src/` - source code for the libraries
- `src-demo/` - demonstration applications
- `src-snippets/` - short snippets of code that weren't incorporated into other parts
