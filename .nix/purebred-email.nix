{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, hedgehog, lens, QuickCheck
, quickcheck-instances, semigroupoids, semigroups, stdenv
, stringsearch, tasty, tasty-golden, tasty-hedgehog, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.4";
  sha256 = "9f19032e956fc62861b5c90ab8e70918d14ce6c958379817d313828029f55793";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    concise deepseq lens semigroupoids semigroups stringsearch text
    time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-insensitive hedgehog lens
    QuickCheck quickcheck-instances semigroups tasty tasty-golden
    tasty-hedgehog tasty-hunit tasty-quickcheck text time
  ];
  homepage = "https://github.com/purebred-mua/purebred-email";
  description = "types and parser for email messages (including MIME)";
  license = stdenv.lib.licenses.agpl3;
}
