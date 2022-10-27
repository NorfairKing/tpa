{ mkDerivation, base, base32, bytestring, cryptonite, directory
, filepath, lib, optparse-applicative, path, path-io, text, time
, yaml
}:
mkDerivation {
  pname = "tpa";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base32 bytestring cryptonite directory filepath
    optparse-applicative path path-io text time yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/tpa#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
