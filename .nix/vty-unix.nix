{ mkDerivation, ansi-terminal, base, blaze-builder, bytestring
, containers, deepseq, lib, microlens, microlens-mtl, microlens-th
, mtl, parsec, stm, terminfo, transformers, unix, utf8-string
, vector, vty
}:
mkDerivation {
  pname = "vty-unix";
  version = "0.2.0.0";
  sha256 = "c2ab67e09edc4bade04e269adc059320e83f68b31e428d11a69b20c4f161ddc1";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base blaze-builder bytestring containers deepseq microlens
    microlens-mtl microlens-th mtl parsec stm terminfo transformers
    unix utf8-string vector vty
  ];
  executableHaskellDepends = [ ansi-terminal base vty ];
  description = "Unix backend for Vty";
  license = lib.licenses.bsd3;
  mainProgram = "vty-unix-build-width-table";
}
