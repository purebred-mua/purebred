# Building purebred
#
# You should be able to simply invoke:
#
# $ nix-build
#
# or, to be explicit:
#
# $ nix-build default.nix
#
# in purebred's root directory in order to build purebred. You'll find the binary under:
#
# $ ls result/bin/purebred
#
# if the build was successful.
#
#
# Choosing a different compiler than the default
#
# In order to choose a different compiler, invoke nix build like so (escaping
# the quotes is needed, since we're passing a string literal):
#
# $ nix-build --arg compiler \"ghc442\"
#
#
# Use as a development environment
#
# $ nix-shell default.nix
#
{ compiler ? null, nixpkgs ? null }:

let
  compilerVersion = if isNull compiler then "ghc863" else compiler;
  config = {
    packageOverrides = super: let self = super.pkgs; in
    {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compilerVersion}" = super.haskell.packages."${compilerVersion}".override {
            overrides = self: super: {
              # Build with latest known stable version.
              purebred = with pkgs.haskell.lib; dontHaddock (self.callPackage ./.nix/purebred.nix { });
              notmuch = self.callPackage ./.nix/hs-notmuch.nix {
                notmuch = pkgs.notmuch;
                talloc = pkgs.talloc;
              };
            };
          };
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
  pkgs = import pkgSrc { inherit config; };
  env = pkgs.haskell.packages.${compilerVersion}.ghcWithPackages (self: [
    self.purebred
  ]);
  nativeBuildTools = with pkgs.haskellPackages; [ cabal-install ghcid hindent pkgs.notmuch pkgs.tmux ];
  in
    if pkgs.lib.inNixShell
    then pkgs.haskell.packages.${compilerVersion}.shellFor {
      withHoogle = true;
      packages = haskellPackages: [ haskellPackages.purebred ];
      nativeBuildInputs = pkgs.haskell.packages.${compilerVersion}.purebred.env.nativeBuildInputs ++ nativeBuildTools;
    }
    else {
        purebred = pkgs.stdenv.mkDerivation {
          name = "purebred-with-packages-${env.version}";
          nativeBuildInputs = [ pkgs.makeWrapper ];
          # This creates a Bash script, which sets the GHC in order for dyre to be
          # able to build the config file.
          buildCommand = ''
            mkdir -p $out/bin
            makeWrapper ${env}/bin/purebred $out/bin/purebred \
            --set NIX_GHC "${env}/bin/ghc"
          '';
          preferLocalBuild = true;
          allowSubstitutes = false;
        };
      }
