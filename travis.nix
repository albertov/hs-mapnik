{ system ? builtins.currentSystem }:

let
  pkgs = import ./nixpkgs { inherit system; };
  withGhc = ghc:
    let myGhc = ghc.override {
      overrides = self: super:
        { hs-mapnik = self.callPackage ./.  { inherit (pkgs) mapnik; }; };
      };
    in myGhc.ghcWithPackages (haskellPackages: with haskellPackages; [
      hs-mapnik
      ]);
in {
  with_801 = withGhc pkgs.haskell.packages.ghc801;
  with_710 = withGhc pkgs.haskell.packages.ghc710;
}
