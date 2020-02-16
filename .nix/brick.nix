{ mkDerivation, base, bytestring, config-ini, containers
, contravariant, data-clist, deepseq, directory, dlist, exceptions
, filepath, microlens, microlens-mtl, microlens-th, QuickCheck
, stdenv, stm, template-haskell, text, text-zipper, transformers
, unix, vector, vty, word-wrap
}:
mkDerivation {
  pname = "brick";
  version = "0.52";
  sha256 = "368dfdbb054fa4b14c091895f1b85742559d6c134081b1546ba3c75ab125e380";
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
