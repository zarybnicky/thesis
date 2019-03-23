{ pkgs ? (import ../../reflex-platform {}).nixpkgs.pkgs,
  ghc ? pkgs.haskellPackages
}:

pkgs.writeShellScriptBin "ghcid-here" ''
  MAIN="Main.main"
  while getopts "hc:" arg; do
    case $arg in
      c)
        MAIN="$OPTARG"
        ;;
      *)
        echo "$0 [-c TESTCOMMAND] [PACKAGENAME]"
        exit 0
        ;;
    esac
  done
  shift $(expr $OPTIND - 1)
  if [ -n $1 ]; then
    PACKAGE=$(basename ''${1})
  fi
  ${ghc.ghcid}/bin/ghcid -W -c "cabal new-repl --ghc-options=-fobject-code ''${PACKAGE:-}" -T "$MAIN"
''
