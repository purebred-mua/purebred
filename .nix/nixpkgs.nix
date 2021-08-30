{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc884" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        purebred = hsuper.callPackage ./purebred.nix { };
        purebred-email = hsuper.callPackage ./purebred-email.nix { };
        purebred-icu = hsuper.callPackage ./purebred-icu.nix { };
        dyre = hsuper.callPackage ./dyre.nix { };
        brick = hsuper.callPackage ./brick.nix { };
      };
    };
  };
  pkgSrc =
    if isNull nixpkgs
    then
    # nixpkgs nixos-unstable - 2021-01-06
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/266dc8c3d052f549826ba246d06787a219533b8f.tar.gz";
      sha256 = "09ydqx2lznixmw8z4cfz1j3k137mh8n3cdpygwqymknhfdjq7lg4";
    }
    else
    nixpkgs;
in
import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
