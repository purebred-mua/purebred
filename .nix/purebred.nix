{ mkDerivation, attoparsec, base, brick, bytestring
, case-insensitive, containers, deepseq, directory, dyre
, exceptions, filepath, haskeline, lens, mime-types, mtl, notmuch
, optparse-applicative, purebred-email, quickcheck-instances
, random, stdenv, stm, tasty, tasty-hunit, tasty-quickcheck
, tasty-tmux, temporary, text, text-zipper, time, transformers
, typed-process, unix, vector, vty, word-wrap
}:
mkDerivation {
  pname = "purebred";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base brick bytestring case-insensitive containers
    deepseq directory dyre exceptions filepath haskeline lens
    mime-types mtl notmuch optparse-applicative purebred-email random
    stm temporary text text-zipper time transformers typed-process
    vector vty word-wrap
  ];
  executableHaskellDepends = [ base brick deepseq lens text ];
  testHaskellDepends = [
    attoparsec base brick bytestring directory filepath lens mtl
    notmuch purebred-email quickcheck-instances tasty tasty-hunit
    tasty-quickcheck tasty-tmux temporary text time typed-process unix
    vector
  ];
  homepage = "https://github.com/purebred-mua/purebred#readme";
  description = "An mail user agent built around notmuch";
  license = stdenv.lib.licenses.agpl3Plus;
}
