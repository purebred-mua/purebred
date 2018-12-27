{ mkDerivation, base, config-ini, containers, contravariant
, data-clist, deepseq, directory, dlist, fetchgit, filepath
, microlens, microlens-mtl, microlens-th, QuickCheck, stdenv, stm
, template-haskell, text, text-zipper, transformers, unix, vector
, vty, word-wrap
}:
mkDerivation {
  pname = "brick";
  version = "0.45";
  src = fetchgit {
    url = "https://github.com/jtdaugherty/brick.git";
    sha256 = "1w9sa46nvr2abf2pzr0n6psdcffv7w62fa0l985scmy9v8qnizjm";
    rev = "2f6f7c3e013d48c72b781fb4d46ce86c66835259";
    fetchSubmodules = true;
  };
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
