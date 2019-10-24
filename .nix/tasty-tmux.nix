{ mkDerivation, base, bytestring, mtl, regex-posix, stdenv, tasty
, tasty-hunit, text, typed-process
}:
mkDerivation {
  pname = "tasty-tmux";
  version = "0.1.0.1";
  sha256 = "029a4992bdf9c6c2af1563ee28c2897063378739fbd7c8bc6139d685660c5d1e";
  libraryHaskellDepends = [
    base bytestring mtl regex-posix tasty tasty-hunit text
    typed-process
  ];
  homepage = "https://github.com/purebred-mua/tasty-tmux";
  description = "Terminal user acceptance testing (UAT) via tmux";
  license = stdenv.lib.licenses.agpl3;
}
