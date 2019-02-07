{ mkDerivation, aeson, base, containers, hedgehog, hspec, jwt, mtl
, random, stdenv, tasty, tasty-hedgehog, tasty-hspec, text, time
, uuid-types
}:
mkDerivation {
  pname = "test-unit";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers jwt mtl random text time uuid-types
  ];
  testHaskellDepends = [
    base hedgehog hspec tasty tasty-hedgehog tasty-hspec uuid-types
  ];
  license = stdenv.lib.licenses.bsd3;
}
