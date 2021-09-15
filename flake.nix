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
  let pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.purebred ]; };
  in rec {
    packages.purebred = pkgs.haskellPackages.purebred;
    defaultPackage = packages.purebred;

#    devShell = nixpkgs.haskellPackages.shellFor {
#      withHoogle = true;
#      packages = haskellPackages: [ haskellPackages.purebred ] ++ icuPackageDep haskellPackages;
#      nativeBuildInputs = haskellPackages.purebred.env.nativeBuildInputs ++ nativeBuildTools;
#    };

  });
}
