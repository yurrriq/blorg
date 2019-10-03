{ mkDerivation, base, filepath, hakyll, split, stdenv, text }:
mkDerivation {
  pname = "blorg";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath hakyll split text ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
  maintainers = with stdenv.lib.maintainers; [ yurrriq ];
}
