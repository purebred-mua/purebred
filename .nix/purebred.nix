{ mkDerivation, attoparsec, base, brick, bytestring
, case-insensitive, containers, deepseq, directory, dyre
, exceptions, filepath, lens, lib, mime-types, mtl, notmuch
, optparse-applicative, purebred-email, quickcheck-instances
, random, stm, stm-delay, tasty, tasty-hunit, tasty-quickcheck
, tasty-tmux, temporary, text, text-zipper, time, typed-process
, unix, vector, vty, word-wrap
}:
mkDerivation {
  pname = "purebred";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base brick bytestring case-insensitive containers
    deepseq directory dyre exceptions filepath lens mime-types mtl
    notmuch optparse-applicative purebred-email random stm stm-delay
    temporary text text-zipper time typed-process vector vty word-wrap
  ];
  testTarget = "unit";
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    attoparsec base brick bytestring directory filepath lens mtl
    notmuch purebred-email quickcheck-instances tasty tasty-hunit
    tasty-quickcheck tasty-tmux temporary text time typed-process unix
    vector
  ];
  homepage = "https://github.com/purebred-mua/purebred#readme";
  description = "An mail user agent built around notmuch";
  license = lib.licenses.agpl3Plus;
  mainProgram = "purebred";
}
