{ pkgs ? (import ../../reflex-platform {}).nixpkgs.pkgs }:
let emacs = pkgs.emacsWithPackages (epkgs: with epkgs; [
  (melpaBuild {
    pname = "dante";
    ename = "dante";
    version = "20190503.645";
    src = pkgs.fetchFromGitHub {
      owner = "jyp";
      repo = "dante";
      rev = "4170ff57f3fd9414915d7a72b805de8727bd81ee";
      sha256 = "0ii3fi8fwnl5q7j7mw165r6sqvyq4g6ymhdlcwb74agkzbssgs87";
    };
    recipe = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/milkypostman/melpa/5afa8226077cbda4b76f52734cf8e0b745ab88e8/recipes/dante";
      sha256 = "1j0qwjshh2227k63vd06bvrsccymqssx26yfzams1xf7bp6y0krs";
      name = "recipe";
    };
    packageRequires = [ company dash f flycheck haskell-mode lcr s ];
  })
  direnv
  flycheck
  haskell-mode
  hasky-extensions
  hindent
  nix-mode
  undo-tree
  use-package
]);
in
pkgs.writeScriptBin "hs-emacs" ''
  #!${pkgs.stdenv.shell}
  exec ${emacs}/bin/emacs -q --load "${./default.el}"
''
