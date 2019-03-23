{ mkDerivation, base, config-ini, containers, contravariant
, data-clist, deepseq, directory, dlist, filepath, microlens
, microlens-mtl, microlens-th, QuickCheck, stdenv, stm
, template-haskell, text, text-zipper, transformers, unix, vector
, vty, word-wrap
}:
mkDerivation {
  pname = "brick";
  version = "0.47";
  sha256 = "47ce722f7a5f0457c97222823e863e455fe8b30710c2a9926d5930a9703892be";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base config-ini containers contravariant data-clist deepseq
    directory dlist filepath microlens microlens-mtl microlens-th stm
    template-haskell text text-zipper transformers unix vector vty
    word-wrap
  ];
  testHaskellDepends = [
    base containers microlens QuickCheck vector
  ];
  homepage = "https://github.com/jtdaugherty/brick/";
  description = "A declarative terminal user interface library";
  license = stdenv.lib.licenses.bsd3;
}
