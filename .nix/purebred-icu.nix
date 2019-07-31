{ mkDerivation, base, case-insensitive, fetchgit, lens, purebred
, purebred-email, stdenv, text, text-icu
}:
mkDerivation {
  pname = "purebred-icu";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/purebred-mua/purebred-icu.git";
    sha256 = "1idvc7flgp3x95c3g3qpk2n6b20dbyvs2wg53f7agx6c1yckm93y";
    rev = "58955e39012a64e05c1820d9aee420d39b4b8ad2";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base case-insensitive lens purebred purebred-email text text-icu
  ];
  homepage = "https://github.com/purebred-mua/purebred-icu";
  description = "ICU charset support for purebred";
  license = stdenv.lib.licenses.agpl3Plus;
}
