{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc863" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        purebred = hsuper.callPackage ./purebred.nix { };
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
      url = "https://github.com/NixOS/nixpkgs/archive/e27e11480323db005ab62ef477eb1fd28b6b62f5.tar.gz";
      sha256 = "0i64wsl20fl92bsqn900nxmmnr1v3088drbwhwpm9lvln42yf23s";
    }
    else
    nixpkgs;
in
  import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
