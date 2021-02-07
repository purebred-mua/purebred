{ mkDerivation, base, bytestring, containers, directory, exceptions
, fetchgit, filepath, HUnit, lib, process, stm, terminfo, text
, transformers, unix
}:
mkDerivation {
  pname = "haskeline";
  version = "0.8.4.1";
  src = fetchgit {
    url = "https://github.com/haskell/haskeline.git";
    sha256 = "0k23hwmcs7wjx7zlzxb2ig3pll1g9f5wyxrgf4gifzrygkci91y6";
    rev = "d8e398babc1834f1b37724bc31cc21a47de14137";
    fetchSubmodules = true;
  };
  configureFlags = [ "-fterminfo" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory exceptions filepath process
    stm terminfo transformers unix
  ];
  executableHaskellDepends = [ base containers ];
  # > Test suite haskeline-tests: RUNNING...
  # > haskeline-tests: which: readCreateProcess: posix_spawnp: does not exist (No such file or directory
  doCheck = false;
  testHaskellDepends = [
    base bytestring containers directory HUnit process text unix
  ];
  homepage = "https://github.com/haskell/haskeline";
  description = "A command-line interface for user input, written in Haskell";
  license = lib.licenses.bsd3;
  mainProgram = "haskeline-examples-Test";
}
