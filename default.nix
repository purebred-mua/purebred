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
# Build with purebred-icu
#
# $ nix-build --arg with-icu true
#
# Use as a development environment
#
# $ nix-shell default.nix
#
{ compiler ? null, nixpkgs ? null, with-icu ? false }@args:

(import .nix/nixpkgs.nix args).purebred-with-packages
