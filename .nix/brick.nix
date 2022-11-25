{ mkDerivation, base, bimap, bytestring, config-ini, containers
, contravariant, data-clist, deepseq, directory, exceptions
, filepath, lib, microlens, microlens-mtl, microlens-th, mtl
, QuickCheck, stm, template-haskell, text, text-zipper, unix
, vector, vty, word-wrap
}:
mkDerivation {
  pname = "brick";
  version = "1.5";
  sha256 = "6290081719d68c149dc9bd0098f36aac235b615334a3510fda89e19bbdb95f4f";
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
