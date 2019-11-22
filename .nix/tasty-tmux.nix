{ mkDerivation, base, bytestring, mtl, regex-posix, stdenv, tasty
, tasty-hunit, text, typed-process
}:
mkDerivation {
  pname = "tasty-tmux";
  version = "0.1.0.2";
  sha256 = "5b290b267aa67b8a20bdb35baf71a6e0394a5f94098687772153dbb0a0557a52";
  libraryHaskellDepends = [
    base bytestring mtl regex-posix tasty tasty-hunit text
    typed-process
  ];
  homepage = "https://github.com/purebred-mua/tasty-tmux";
  description = "Terminal user acceptance testing (UAT) via tmux";
  license = stdenv.lib.licenses.agpl3;
}
