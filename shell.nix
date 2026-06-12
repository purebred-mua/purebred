{ with-icu ? false }:
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  };
  flake = (import flake-compat { src = ./.; }).defaultNix;
in
flake.devShells.${builtins.currentSystem}.${if with-icu then "with-icu" else "default"}
