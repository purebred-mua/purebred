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
{ compiler ? "ghc822" }:

let
  config = {
    packageOverrides = super: let self = super.pkgs; in
    {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compiler}" = super.haskell.packages."${compiler}".override {
            overrides = self: super: {
              # Build with latest known stable version.
              purebred = with pkgs.haskell.lib; dontHaddock (self.callPackage ./.nix/purebred.nix { });
              purebred-email = self.callPackage ./.nix/purebred-email.nix { };
              # Version pins since those versions were bleeding edge and not on
              # hackage/stackage or Nix yet. Remove these once builds without
              # these pass.
              notmuch = self.callPackage ./.nix/hs-notmuch.nix {
                notmuch = pkgs.notmuch;
                talloc = pkgs.talloc;
              };
              brick = self.callPackage ./.nix/brick.nix { };
              vty = self.callPackage ./.nix/vty.nix { };
              Cabal = self.callPackage ./.nix/Cabal.nix { };
            };
          };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
  env = pkgs.haskell.packages.${compiler}.ghcWithPackages (self: [
    self.purebred
  ]);
  in
    if pkgs.lib.inNixShell
    then pkgs.haskell.packages.${compiler}.purebred.env
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
