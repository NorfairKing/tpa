{ mkDerivation, autodocodec, base, base32, bytestring, cryptonite
, directory, filepath, lib, opt-env-conf, path, path-io, text, time
, yaml
}:
mkDerivation {
  pname = "tpa";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base base32 bytestring cryptonite directory filepath
    opt-env-conf path path-io text time yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/tpa#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "tpa";
}
