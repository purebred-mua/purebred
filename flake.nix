{
  description = "Purebred development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {

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

    packages.x86_64-linux.purebred =
      let pkgs = import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.purebred ]; };
      in pkgs.haskellPackages.purebred;
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.purebred;

#    devShell.x86_64-linux = nixpkgs.haskellPackages.shellFor {
#      withHoogle = true;
#      packages = haskellPackages: [ haskellPackages.purebred ] ++ icuPackageDep haskellPackages;
#      nativeBuildInputs = haskellPackages.purebred.env.nativeBuildInputs ++ nativeBuildTools;
#    };

  };
}
