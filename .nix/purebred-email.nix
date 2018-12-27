{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, fetchgit, lens, QuickCheck
, quickcheck-instances, semigroupoids, semigroups, stdenv
, stringsearch, tasty, tasty-golden, tasty-hunit, tasty-quickcheck
, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/purebred-mua/purebred-email.git";
    sha256 = "0vc2bcagd60mqfx96k9s83kfkq3wcj5ibacr4a1n3xh6gcr1qg4i";
    rev = "89928795c06abc48cbc7ac4eb00422fe176eab32";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    concise deepseq lens semigroupoids semigroups stringsearch text
    time
  ];
  executableHaskellDepends = [
    attoparsec base bytestring semigroups
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
