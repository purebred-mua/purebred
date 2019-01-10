{ mkDerivation, base, bytestring, c2hs, deepseq, mtl, notmuch
, profunctors, stdenv, tagged, talloc, text, time
}:
mkDerivation {
  pname = "notmuch";
  version = "0.2.0.0";
  sha256 = "feeafa5bdc00c1036755c92d72e7b69cb388ae9bab5064410655007cc8216ff0";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring deepseq mtl profunctors tagged text time
  ];
  librarySystemDepends = [ notmuch talloc ];
  libraryToolDepends = [ c2hs ];
  homepage = "https://github.com/purebred-mua/hs-notmuch";
  description = "Haskell binding to Notmuch, the mail indexer";
  license = stdenv.lib.licenses.gpl3;
}
