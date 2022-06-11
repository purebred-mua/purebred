{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc901" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        purebred = hsuper.callPackage ./purebred.nix { };
        purebred-email = hsuper.callPackage ./purebred-email.nix { };
        purebred-icu = hsuper.callPackage ./purebred-icu.nix { };
        dyre = hsuper.callPackage ./dyre.nix { };
        brick = hsuper.callPackage ./brick.nix { };
        typed-process = hsuper.callPackage ./typed-process.nix { };
      };
    };
  };
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  pkgSrc =
    if isNull nixpkgs
    then
    # nixpkgs nixos-unstable - 2021-01-06
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs.locked.narHash;
    }
    else
    nixpkgs;
in
import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
