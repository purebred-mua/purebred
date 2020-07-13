{ mkDerivation, ansi-terminal, base, binary, blaze-builder
, bytestring, Cabal, containers, deepseq, directory, filepath
, hashable, HUnit, microlens, microlens-mtl, microlens-th, mtl
, parallel, parsec, QuickCheck, quickcheck-assertions, random
, smallcheck, stdenv, stm, string-qq, terminfo, test-framework
, test-framework-hunit, test-framework-smallcheck, text
, transformers, unix, utf8-string, vector
}:
mkDerivation {
  pname = "vty";
  version = "5.29";
  sha256 = "eab7107cf3a0c56356aa75b2b7f98874b5725d9ac2031fad3695bf8c77404bbe";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base binary blaze-builder bytestring containers
    deepseq directory filepath hashable microlens microlens-mtl
    microlens-th mtl parallel parsec stm terminfo text transformers
    unix utf8-string vector
  ];
  executableHaskellDepends = [
    base containers directory filepath microlens microlens-mtl mtl
  ];
  testHaskellDepends = [
    base blaze-builder bytestring Cabal containers deepseq HUnit
    microlens microlens-mtl mtl QuickCheck quickcheck-assertions random
    smallcheck stm string-qq terminfo test-framework
    test-framework-hunit test-framework-smallcheck text unix
    utf8-string vector
  ];
  homepage = "https://github.com/jtdaugherty/vty";
  description = "A simple terminal UI library";
  license = stdenv.lib.licenses.bsd3;
}
