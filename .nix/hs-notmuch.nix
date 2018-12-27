{ mkDerivation, base, bytestring, c2hs, containers, deepseq
, fetchgit, mtl, notmuch, profunctors, stdenv, tagged, talloc, text
, time
}:
mkDerivation {
  pname = "notmuch";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/purebred-mua/hs-notmuch.git";
    sha256 = "0p9q48lpvib2l2fy9k2s77d7rxm0vkiclji3s016ic7jpyqpmyv0";
    rev = "2d2ceedbe3d612ec273477fed0217228b3f35914";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring deepseq mtl profunctors tagged text time
  ];
  librarySystemDepends = [ notmuch talloc ];
  libraryToolDepends = [ c2hs ];
  executableHaskellDepends = [ base bytestring containers mtl ];
  homepage = "https://github.com/purebred-mua/hs-notmuch";
  description = "Haskell binding to Notmuch, the mail indexer";
  license = stdenv.lib.licenses.gpl3;
}
