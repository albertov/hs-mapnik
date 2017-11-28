{ lib, stdenv, mkDerivation, base, bytestring, Cabal, hs-mapnik
, hs-mapnik-bindings, hs-mapnik-setup, hspec, hspec-core, inline-c
, inline-c-cpp, lens, QuickCheck, quickcheck-io, text
, unordered-containers, vector, mapnik-vector-tile
}:
mkDerivation {
  pname = "hs-mapnik-vectortile";
  version = "0.2.0.0";
  src = lib.cleanSource ./.;
  setupHaskellDepends = [ base Cabal hs-mapnik-setup ];
  libraryHaskellDepends = [
    base bytestring hs-mapnik hs-mapnik-bindings inline-c inline-c-cpp
    lens text unordered-containers vector mapnik-vector-tile
  ];
  testHaskellDepends = [
    base bytestring hs-mapnik hs-mapnik-bindings hspec hspec-core lens
    QuickCheck quickcheck-io text unordered-containers
  ];
  configureFlags = [
    "--extra-lib-dirs=${mapnik-vector-tile}/lib"
    "--extra-include-dirs=${mapnik-vector-tile}/include"
    ];
  homepage = "https://github.com/albertov/hs-mapnik#readme";
  description = "Haskell bindings for Mapnik VectorTile";
  license = stdenv.lib.licenses.bsd3;
}
