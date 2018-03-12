selfPkgs: superPkgs:
let inherit (selfPkgs.lib) composeExtensions;
  inherit (hsLib) dontHaddock;

  hsLib = import "${superPkgs.path}/pkgs/development/haskell-modules/lib.nix"
            { pkgs=superPkgs; inherit (superPkgs) lib; };
in
{
  haskellPackages = superPkgs.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: {})) (self: super: {
      mkDerivation = drv: dontHaddock (super.mkDerivation drv);
      });
  });
}

