{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc884" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        purebred = hsuper.callPackage ./purebred.nix { };
        purebred-email = hsuper.callPackage ./purebred-email.nix { };
        purebred-icu = hsuper.callPackage ./purebred-icu.nix { };
        tasty-tmux = hsuper.callPackage ./tasty-tmux.nix { };
        notmuch = hsuper.callPackage ./hs-notmuch.nix {
          notmuch = self.pkgs.notmuch;
          talloc = self.pkgs.talloc;
        };
        brick = hsuper.callPackage ./brick.nix {};
        vty = hsuper.callPackage ./vty.nix {};
      };
    };
  };
  pkgSrc =
    if isNull nixpkgs
    then
    # nixpkgs nixos-unstable - 2021-01-06
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/1a57d96edd156958b12782e8c8b6a374142a7248.tar.gz";
      sha256 = "1qdh457apmw2yxbpi1biwl5x5ygaw158ppff4al8rx7gncgl10rd";
    }
    else
    nixpkgs;
in
import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
