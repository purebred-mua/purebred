{ mkDerivation, base, bytestring, mtl, regex-posix, stdenv, tasty
, tasty-hunit, text, typed-process
}:
mkDerivation {
  pname = "tasty-tmux";
  version = "0.1.0.0";
  sha256 = "0886b212d0e58820175fb99fac94d469d9dcb28454065a21223b4bb79d60fdcc";
  libraryHaskellDepends = [
    base bytestring mtl regex-posix tasty tasty-hunit text
    typed-process
  ];
  homepage = "https://github.com/purebred-mua/tasty-tmux";
  description = "Terminal user acceptance testing (UAT) via tmux";
  license = stdenv.lib.licenses.agpl3;
}
