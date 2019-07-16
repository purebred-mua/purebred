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

with (import .nix/nixpkgs.nix { inherit compiler nixpkgs; });

let
  env = haskellPackages.ghcWithPackages (self: [
    self.purebred
  ]);
  nativeBuildTools = with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    ghcid
    hindent
    hlint
    pkgs.notmuch
    pkgs.tmux
  ];
  in
    if pkgs.lib.inNixShell
    then haskellPackages.shellFor {
      withHoogle = true;
      packages = haskellPackages: [ haskellPackages.purebred ];
      nativeBuildInputs = haskellPackages.purebred.env.nativeBuildInputs ++ nativeBuildTools;
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
