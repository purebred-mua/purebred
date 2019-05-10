{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc864" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        purebred = hsuper.callPackage ./purebred.nix { };
      };
    };
  };
  pkgSrc =
    if isNull nixpkgs
    then
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/28edabd88ed231316f55de0c01d15fad56485cf0.tar.gz";
      sha256 = "0gbksg3icp04iri5cbchgqiw3n1lfs7i9xzf3v8wsr2f7y8zx45b";
    }
    else
    nixpkgs;
in
  import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
