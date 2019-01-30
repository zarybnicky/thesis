{ mkDerivation, base, ghcjs-dom, jsaddle, jsaddle-warp, lens
, reflex-dom-core, stdenv
}:
mkDerivation {
  pname = "trivial-client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base ghcjs-dom jsaddle jsaddle-warp lens reflex-dom-core
  ];
  license = stdenv.lib.licenses.bsd3;
}
