{ mkDerivation, base, lib, vty, vty-unix }:
mkDerivation {
  pname = "vty-crossplatform";
  version = "0.4.0.0";
  sha256 = "35e5433512b883e83aa8bb8c3475221174445a87e51f162b2ce07f9cf1eb3c1a";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base vty vty-unix ];
  description = "Cross-platform support for Vty";
  license = lib.licenses.bsd3;
}
