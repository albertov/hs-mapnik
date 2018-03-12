{ system ? builtins.currentSystem
}:

let
  revision = "5065d28a1cc84d9e50a2ff16eb5b533b9dcd9be9";
  nixpkgsUrl =
    "https://github.com/NixOS/nixpkgs/archive/${revision}.tar.gz";
  pkgs = import (fetchTarball nixpkgsUrl)
    { overlays = [
        (import ./overlays/haskellPackages.nix)
        (import ./overlays/systemPackages.nix)
      ];
    };
in
  pkgs.haskellPackages.ghcWithPackages (ps: with ps;
    [ hs-mapnik
      hs-mapnik-bindings
      hs-mapnik-purs
      hs-mapnik-setup
      hs-mapnik-swagger
      hs-mapnik-vectortile
    ])
