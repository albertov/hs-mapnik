{ mkDerivation, fetchFromGitHub, base, Cabal, directory
, filepath, hspec, hspec-core, process, stdenv
, bytestring, icu, harfbuzz, mapnik, boost, inline-c-cpp
, inline-c, hs-mapnik, hs-mapnik-setup, text
}:
mkDerivation {
  pname = "hs-mapnik-bindings";
  version = "0.1.0.0";
  src = ./.;
  doHaddock = false;
  preConfigure = "export MAPNIK_CONFIG=${mapnik}/bin/mapnik-config";
  setupHaskellDepends = [ base Cabal directory filepath process ];
  libraryHaskellDepends = [ base bytestring inline-c-cpp inline-c hs-mapnik hs-mapnik-setup ];
  librarySystemDepends = [ mapnik boost.dev icu.dev harfbuzz.dev ];
  testHaskellDepends = [
    base bytestring hspec hspec-core text hs-mapnik
  ];
  homepage = "https://github.com/albertov/hs-mapnik#readme";
  description = "Haskel bindings for Mapnik";
  license = stdenv.lib.licenses.bsd3;
}
