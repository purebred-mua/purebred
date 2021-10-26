{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, hedgehog, lens, lib
, QuickCheck, quickcheck-instances, random, semigroupoids
, semigroups, stringsearch, tasty, tasty-golden, tasty-hedgehog
, tasty-hunit, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.5";
  sha256 = "412dc455ff244f864b66ee00ad0fc5f9986cdbc41e60c62f7468108dddf47645";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    concise deepseq lens random semigroupoids semigroups stringsearch
    text time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-insensitive hedgehog lens
    QuickCheck quickcheck-instances random semigroups tasty
    tasty-golden tasty-hedgehog tasty-hunit tasty-quickcheck text time
  ];
  homepage = "https://github.com/purebred-mua/purebred-email";
  description = "types and parser for email messages (including MIME)";
  license = lib.licenses.agpl3Plus;
}
