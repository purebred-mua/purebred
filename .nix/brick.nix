{ mkDerivation, base, bimap, bytestring, config-ini, containers
, contravariant, data-clist, deepseq, directory, exceptions
, filepath, lib, microlens, microlens-mtl, microlens-th, mtl
, QuickCheck, stm, template-haskell, text, text-zipper, unix
, vector, vty, word-wrap
}:
mkDerivation {
  pname = "brick";
  version = "2.1.1";
  sha256 = "30280d6f7130eb3e6cbf5a55465a06a825169cb536d3b2e91883aec23532b31e";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bimap bytestring config-ini containers contravariant
    data-clist deepseq directory exceptions filepath microlens
    microlens-mtl microlens-th mtl stm template-haskell text
    text-zipper unix vector vty word-wrap
  ];
  testHaskellDepends = [
    base containers microlens QuickCheck vector vty
  ];
  homepage = "https://github.com/jtdaugherty/brick/";
  description = "A declarative terminal user interface library";
  license = lib.licenses.bsd3;
}
