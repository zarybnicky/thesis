{ mkDerivation, aeson, aeson-pretty, base, bytestring
, servant-server, servant-swagger, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "trivial-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring servant-server servant-swagger
    text wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
