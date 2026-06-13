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
# Build with purebred-icu
#
# $ nix-build --arg with-icu true
#
# Use as a development environment
#
# $ nix-shell default.nix
#
{ with-icu ? false }:
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  };
  flake = (import flake-compat { src = ./.; }).defaultNix;
  packages = flake.packages.${builtins.currentSystem};
in
if with-icu
then packages.purebred-with-packages-icu
else packages.purebred-with-packages


