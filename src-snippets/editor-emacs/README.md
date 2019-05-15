# Emacs configuration

This Emacs configuration is set up to work with the practices used when
developing the libraries and applications in this thesis. To run it, do

```
cd src-snippets/editor-emacs
nix-build
result/bin/hs-emacs
```

That will open a new Emacs instance with a minimal configuration (which can be
seen in this directory in the file `default.el`) with a configured `dante-mode`,
`haskell-mode`, and `flycheck-mode`.

To work on a package, first run `direnv allow` in the package's root directory,
which will download and compile all dependencies, and make them available in the
shell. If you don't do it first, Emacs will tell you that direnv is disabled in
the directory and will not be able to run syntax (and other) error checking.
After all dependencies are installed, you can run cabal and other compiler
commands, and Emacs will also start a cabal process in the background when you
first enter a project using `dante` and `flycheck` - for more information on
this, see https://github.com/jyp/dante/.
