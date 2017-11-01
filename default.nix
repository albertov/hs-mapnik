{ mkDerivation, fetchFromGitHub, base, bytestring, c2hs, Cabal, directory, either
, filepath, hspec, hspec-core, process, stdenv
, transformers, icu, harfbuzz, mapnik, boost, inline-c-cpp
}:
mkDerivation {
  pname = "hs-mapnik";
  version = "0.1.0.0";
  src = ./.;
  preConfigure = "export MAPNIK_CONFIG=${mapnik}/bin/mapnik-config";
  setupHaskellDepends = [ base Cabal directory filepath process ];
  libraryHaskellDepends = [ base bytestring either transformers inline-c-cpp ];
  librarySystemDepends = [ c2hs mapnik boost.dev icu.dev harfbuzz.dev ];
  testHaskellDepends = [
    base bytestring hspec hspec-core transformers
  ];
  homepage = "https://github.com/albertov/hs-mapnik#readme";
  description = "Haskel bindings for Mapnik";
  license = stdenv.lib.licenses.bsd3;
}
