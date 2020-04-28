{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, hedgehog, lens, QuickCheck
, quickcheck-instances, semigroupoids, semigroups, stdenv
, stringsearch, tasty, tasty-golden, tasty-hedgehog, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.4.1";
  sha256 = "e94e0696d6d92727a837519dfe1d9684bb650d54a35ef77d13175dfa40ab6d3a";
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
