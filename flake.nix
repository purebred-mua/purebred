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
    mkShell = with-icu: pkgs.haskellPackages.shellFor {
      withHoogle = true;
      packages = hp: [ hp.purebred ] ++ (if with-icu then [hp.purebred-icu] else []);
      nativeBuildInputs = pkgs.haskellPackages.purebred.env.nativeBuildInputs ++ nativeBuildTools;
    };
    mkPurebredWithPackages = with-icu:
      let
        envPackages = self: if with-icu then [ self.purebred-icu ] else [ self.purebred ];
        env = pkgs.haskellPackages.ghcWithPackages envPackages;
      in pkgs.stdenv.mkDerivation {
        name = "purebred-with-packages-${env.version}";
        nativeBuildInputs = [ pkgs.makeWrapper ];
        # This creates a Bash script, which sets the GHC in order for dyre to be
        # able to build the config file.
        buildCommand = ''
          mkdir -p $out/bin
          makeWrapper ${env}/bin/purebred $out/bin/purebred \
          --set NIX_GHC "${env}/bin/ghc"
          makeWrapper ${env}/bin/purebred-dump-keybinding $out/bin/purebred-dump-keybinding \
          --set NIX_GHC "${env}/bin/ghc"
        '';
        preferLocalBuild = true;
        allowSubstitutes = false;
      };
  in rec {
    packages = {
      purebred-with-packages = mkPurebredWithPackages false;
      purebred-with-packages-icu = mkPurebredWithPackages true;
      purebred = pkgs.haskellPackages.purebred;
      purebred-email = pkgs.haskellPackages.purebred-email;
      purebred-icu = pkgs.haskellPackages.purebred-icu;
      dyre = pkgs.haskellPackages.dyre;
      brick = pkgs.haskellPackages.brick;
      # shell "packages" for `nix develop .#shell-with/out-icu`
      shell-without-icu = mkShell false;
      shell-with-icu = mkShell true;
    };
    defaultPackage = packages.purebred-with-packages;
    devShell = packages.shell-without-icu;
  });
}
