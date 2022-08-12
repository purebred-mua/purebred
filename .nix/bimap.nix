{ mkDerivation, base, containers, deepseq, exceptions, lib
, QuickCheck, template-haskell
}:
mkDerivation {
  pname = "bimap";
  version = "0.5.0";
  sha256 = "b0b44b0f2eaceb83f46dfa3d1747e080c45204c64d18bb9e63747299266f0c95";
  libraryHaskellDepends = [ base containers deepseq exceptions ];
  testHaskellDepends = [
    base containers deepseq exceptions QuickCheck template-haskell
  ];
  homepage = "https://github.com/joelwilliamson/bimap";
  description = "Bidirectional mapping between two key types";
  license = lib.licenses.bsd3;
}
