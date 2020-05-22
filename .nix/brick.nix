{ mkDerivation, base, bytestring, config-ini, containers
, contravariant, data-clist, deepseq, directory, dlist, exceptions
, filepath, microlens, microlens-mtl, microlens-th, QuickCheck
, stdenv, stm, template-haskell, text, text-zipper, transformers
, unix, vector, vty, word-wrap
}:
mkDerivation {
  pname = "brick";
  version = "0.54";
  sha256 = "1bce8104859986a3f871751de34f1876a3ece7632fe981d0cb267a1e00074281";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring config-ini containers contravariant data-clist
    deepseq directory dlist exceptions filepath microlens microlens-mtl
    microlens-th stm template-haskell text text-zipper transformers
    unix vector vty word-wrap
  ];
  testHaskellDepends = [
    base containers microlens QuickCheck vector
  ];
  homepage = "https://github.com/jtdaugherty/brick/";
  description = "A declarative terminal user interface library";
  license = stdenv.lib.licenses.bsd3;
}
