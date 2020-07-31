{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, hedgehog, lens, QuickCheck
, quickcheck-instances, semigroupoids, semigroups, stdenv
, stringsearch, tasty, tasty-golden, tasty-hedgehog, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.4.2";
  sha256 = "7e611e912d7fbaa482eb059481f4074a4053bcda7bbfd9fb602476cbac8b92a1";
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
