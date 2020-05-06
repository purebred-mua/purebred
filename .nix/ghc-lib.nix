{ mkDerivation, alex, array, base, binary, bytestring, containers
, deepseq, directory, filepath, ghc-lib-parser, ghc-prim, happy
, hpc, pretty, process, stdenv, time, transformers, unix
}:
mkDerivation {
  pname = "ghc-lib";
  version = "8.10.1.20200412";
  sha256 = "b1623069271466f77286182a6bd3f5db54cfc4920bb4309396cc52e77f04c878";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    ghc-lib-parser ghc-prim hpc pretty process time transformers unix
  ];
  libraryToolDepends = [ alex happy ];
  homepage = "https://github.com/digital-asset/ghc-lib";
  description = "The GHC API, decoupled from GHC versions";
  license = stdenv.lib.licenses.bsd3;
}
