selfPkgs: superPkgs:
let
  inherit (selfPkgs.lib) composeExtensions;
  inherit (hsLib) overrideCabal dontHaddock;

  hsLib = import "${superPkgs.path}/pkgs/development/haskell-modules/lib.nix"
            { pkgs=selfPkgs; inherit (selfPkgs) lib; };

  callCabal2nix = a: b: c:
    dontHaddock (selfPkgs.haskellPackages.callCabal2nix a b c);
in
{
  haskellPackages = superPkgs.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: {})) (self: super: {
      hs-mapnik = callCabal2nix "hs-mapnik" ../../pure {};

      hs-mapnik-bindings =
        let drv = callCabal2nix "hs-mapnik-bindings" ../../bindings {};
        in overrideCabal drv
          { librarySystemDepends = with selfPkgs; [ mapnik boost harfbuzz icu ]; };

      hs-mapnik-purs = callCabal2nix "hs-mapnik-purs" ../../purs {};

      hs-mapnik-server = callCabal2nix "hs-mapnik-server" ../../server {};

      hs-mapnik-setup = callCabal2nix "hs-mapnik-setup" ../../setup {};

      hs-mapnik-swagger = callCabal2nix "hs-mapnik-swagger" ../../swagger {};

      hs-mapnik-vectortile =
        let drv = dontHaddock (self.callPackage ../../vectortile {});
        in overrideCabal drv
          { librarySystemDepends = with selfPkgs;
            [ boost harfbuzz icu mapnik zlib protozero mapbox-geometry mapbox-wagyu ]; };
    });
  });
}
