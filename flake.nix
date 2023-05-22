{
  description = "Purebred development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: {

    overlays.default = final: prev:
    let
      overlays = import .nix/overlays.nix;
    in
    prev.lib.composeManyExtensions overlays final prev;

  } // utils.lib.eachSystem ["x86_64-linux"] (system:
  let
    pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
  in rec {
    packages = {
      default = self.packages.${system}.purebred-with-packages-icu;
      purebred-with-packages = pkgs.purebred-with-packages;
      purebred-with-packages-icu = pkgs.purebred-with-packages-icu;
      purebred = pkgs.haskellPackages.purebred;
      purebred-email = pkgs.haskellPackages.purebred-email;
      purebred-icu = pkgs.haskellPackages.purebred-icu;
      dyre = pkgs.haskellPackages.dyre;
      brick = pkgs.haskellPackages.brick;
      # shell "packages" for `nix develop .#shell-with/out-icu`
      shell-without-icu = pkgs.make-purebred-shell false;
      shell-with-icu = pkgs.make-purebred-shell true;
    };
    devShells.default = packages.shell-without-icu;
  });
}
