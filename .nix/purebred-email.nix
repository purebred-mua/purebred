{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, lens, QuickCheck
, quickcheck-instances, semigroupoids, semigroups, stdenv
, stringsearch, tasty, tasty-golden, tasty-hunit, tasty-quickcheck
, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.2.0.0";
  sha256 = "c1b48667832b7d58a55f151b9313eb37008c4ddd5e2b9c5d5db1718a918a4dca";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    concise deepseq lens semigroupoids semigroups stringsearch text
    time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-insensitive lens QuickCheck
    quickcheck-instances semigroups tasty tasty-golden tasty-hunit
    tasty-quickcheck text time
  ];
  homepage = "https://github.com/purebred-mua/purebred-email";
  description = "types and parser for email messages (including MIME)";
  license = stdenv.lib.licenses.agpl3;
}
