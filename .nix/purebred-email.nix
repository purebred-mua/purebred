{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, hedgehog, lens, lib
, quickcheck-instances, random, semigroupoids, stringsearch, tasty
, tasty-golden, tasty-hedgehog, tasty-hunit, tasty-quickcheck, text
, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.6";
  sha256 = "05a539b91afc187bbd0e03c6a36b85707405e1d7592bacddccda11e0bf970945";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    concise deepseq lens random semigroupoids stringsearch text time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-insensitive hedgehog lens
    quickcheck-instances random tasty tasty-golden tasty-hedgehog
    tasty-hunit tasty-quickcheck text time
  ];
  homepage = "https://github.com/purebred-mua/purebred-email";
  description = "types and parser for email messages (including MIME)";
  license = lib.licenses.agpl3Plus;
}
