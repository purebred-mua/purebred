{ mkDerivation, base, bytestring, config-ini, containers
, contravariant, data-clist, deepseq, directory, dlist, exceptions
, filepath, microlens, microlens-mtl, microlens-th, QuickCheck
, stdenv, stm, template-haskell, text, text-zipper, transformers
, unix, vector, vty, word-wrap
}:
mkDerivation {
  pname = "brick";
  version = "0.55";
  sha256 = "de09e03e8223ed10d0bddcbb836726b5849b7f9a092ab7b05942952311dca158";
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
