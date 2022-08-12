{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, hedgehog, lens, lib
, QuickCheck, quickcheck-instances, random, semigroupoids
, stringsearch, tasty, tasty-golden, tasty-hedgehog, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.5.1";
  sha256 = "b518af3f8d4077f179a7acc5afd16bb5e58468d1b357196ac2badcb522f8c4bc";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    concise deepseq lens random semigroupoids stringsearch text time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-insensitive hedgehog lens
    QuickCheck quickcheck-instances random tasty tasty-golden
    tasty-hedgehog tasty-hunit tasty-quickcheck text time
  ];
  homepage = "https://github.com/purebred-mua/purebred-email";
  description = "types and parser for email messages (including MIME)";
  license = lib.licenses.agpl3Plus;
}
