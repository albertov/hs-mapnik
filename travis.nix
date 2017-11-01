{ system ? builtins.currentSystem
}:

let
  revision = "36a4dc392a40e06973a64b7666b179f4a021682a";
  nixpkgsUrl =
    "https://github.com/NixOS/nixpkgs-channels/archive/${revision}.tar.gz";
  pkgs = import (fetchTarball nixpkgsUrl) {};
  withGhc = ghc:
    let myGhc = ghc.override {
      overrides = self: super:
        {
          hs-mapnik = self.callPackage ./.  { inherit (pkgs) mapnik; };
          inline-c = super.inline-c_0_6_0_5;
          inline-c-cpp = super.inline-c-cpp_0_2_1_0;
        };
      };
    in myGhc.ghcWithPackages (haskellPackages: with haskellPackages; [
      hs-mapnik
      ]);
in {
  with_821 = withGhc pkgs.haskell.packages.ghc821;
}
