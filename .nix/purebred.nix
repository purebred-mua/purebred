{ mkDerivation, async, attoparsec, base, brick, bytestring
, case-insensitive, containers, deepseq, directory, dyre
, exceptions, filepath, haskeline, lens, lib, mime-types
, monad-loops, mtl, notmuch, optparse-applicative, purebred-email
, quickcheck-instances, random, stm, stm-delay, tasty, tasty-hunit
, tasty-quickcheck, tasty-tmux, temporary, text, text-zipper, time
, transformers, typed-process, unix, vector, vty, word-wrap
}:
mkDerivation {
  pname = "purebred";
  version = "2022.1";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base brick bytestring case-insensitive containers
    deepseq directory dyre exceptions filepath haskeline lens
    mime-types monad-loops mtl notmuch optparse-applicative
    purebred-email random stm stm-delay temporary text text-zipper time
    transformers typed-process vector vty word-wrap
  ];
  testTarget = "unit";
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring directory filepath lens mtl notmuch purebred-email
    quickcheck-instances tasty tasty-hunit tasty-quickcheck tasty-tmux
    temporary text time typed-process unix vector
  ];
  homepage = "https://github.com/purebred-mua/purebred#readme";
  description = "An mail user agent built around notmuch";
  license = lib.licensesSpdx."AGPL-3.0-or-later";
  mainProgram = "purebred";
}
