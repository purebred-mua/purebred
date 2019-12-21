{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc865" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        purebred = hsuper.callPackage ./purebred.nix { };
        purebred-email = hsuper.callPackage ./purebred-email.nix { };
        purebred-icu = hsuper.callPackage ./purebred-icu.nix { };
        tasty-tmux = hsuper.callPackage ./tasty-tmux.nix { };
        brick = hsuper.callPackage ./brick.nix {};
        notmuch = hsuper.callPackage ./hs-notmuch.nix {
          notmuch = self.pkgs.notmuch;
          talloc = self.pkgs.talloc;
        };
      };
    };
  };
  pkgSrc =
    if isNull nixpkgs
    then
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/b9cb3b2fb2f45ac8f3a8f670c90739eb34207b0e.tar.gz";
      sha256 = "1cpjmsa2lwfxg55ac02w9arbd3y5617d19x91sd1fq521jqbnnpc";
    }
    else
    nixpkgs;
in
  import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
