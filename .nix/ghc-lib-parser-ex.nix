{ mkDerivation, base, bytestring, containers, directory, extra
, filepath, ghc-lib-parser, stdenv, tasty, tasty-hunit, uniplate
}:
mkDerivation {
  pname = "ghc-lib-parser-ex";
  version = "8.10.0.6";
  sha256 = "2a4c67cf04ffb194c79b4fa50dab29f9a56f5e7708d6f2771bebbafe4f338150";
  libraryHaskellDepends = [
    base bytestring containers ghc-lib-parser uniplate
  ];
  testHaskellDepends = [
    base directory extra filepath ghc-lib-parser tasty tasty-hunit
  ];
  homepage = "https://github.com/shayne-fletcher/ghc-lib-parser-ex#readme";
  description = "Algorithms on GHC parse trees";
  license = stdenv.lib.licenses.bsd3;
}
