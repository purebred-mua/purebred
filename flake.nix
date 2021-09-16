{
  description = "Purebred development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: {

    overlays = {
      purebred = final: prev: with prev.haskell.lib; {
        # TODO handle different compilers
        haskellPackages = prev.haskell.packages.ghc884.override {
          overrides = hself: hsuper: {
            purebred = hsuper.callPackage .nix/purebred.nix { };
            purebred-email = hsuper.callPackage .nix/purebred-email.nix { };
            purebred-icu = hsuper.callPackage .nix/purebred-icu.nix { };
            dyre = hsuper.callPackage .nix/dyre.nix { };
            brick = hsuper.callPackage .nix/brick.nix { };
          };
        };
      };
    };
  } // utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.purebred ]; };
    nativeBuildTools = with pkgs.haskellPackages; [
      cabal-install
      cabal2nix
      ghcid
      hlint
      haskell-language-server
      ormolu
      hie-bios
      pkgs.notmuch
      pkgs.tmux
      pkgs.gnumake
      pkgs.asciidoctor
      pkgs.python3Packages.pygments
    ];
  in rec {
    packages = {
      purebred = pkgs.haskellPackages.purebred;
      purebred-email = pkgs.haskellPackages.purebred-email;
      purebred-icu = pkgs.haskellPackages.purebred-icu;
      dyre = pkgs.haskellPackages.dyre;
      brick = pkgs.haskellPackages.brick;
    };
    defaultPackage = packages.purebred;

    devShell = pkgs.haskellPackages.shellFor {
      withHoogle = true;
      packages = haskellPackages: [ haskellPackages.purebred ] ;#++ icuPackageDep haskellPackages;
      nativeBuildInputs = defaultPackage.env.nativeBuildInputs ++ nativeBuildTools;
    };

  });
}
