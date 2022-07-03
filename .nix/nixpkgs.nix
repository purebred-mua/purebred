{ compiler ? null, nixpkgs ? null}:

let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  pkgSrc =
    if isNull nixpkgs
    then
    # nixpkgs nixos-unstable - 2021-01-06
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs.locked.narHash;
    }
    else
    nixpkgs;
    haskellPackagesOverlay = import ./overlays.nix;
in
import pkgSrc { overlays = haskellPackagesOverlay; }
