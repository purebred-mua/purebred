{ mkDerivation, attoparsec, base, brick, bytestring
  , case-insensitive, containers, deepseq, directory, dyre
  , exceptions, filepath, ini, lens, lib, mime-types, mtl, notmuch
  , optparse-applicative, purebred-email, quickcheck-instances
  , random, regex-posix, stm, tasty, tasty-hunit
  , tasty-quickcheck, temporary, text, text-zipper, time
  , typed-process, vector, vty
  , Cabal, tasty-tmux
}:
mkDerivation {
  pname = "purebred";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ Cabal ];
  libraryHaskellDepends = [
    attoparsec base brick bytestring case-insensitive containers
    deepseq directory dyre exceptions filepath lens mime-types mtl
    notmuch optparse-applicative purebred-email random temporary text
    text-zipper time typed-process vector vty
  ];
  testTarget = "unit";
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base brick bytestring directory filepath ini lens mtl notmuch
    purebred-email quickcheck-instances regex-posix stm tasty
    tasty-hunit tasty-quickcheck temporary text time typed-process
    vector tasty-tmux
  ];
  homepage = "https://github.com/githubuser/purebred#readme";
  description = "An mail user agent built around notmuch";
  license = lib.licenses.agpl3;
}
