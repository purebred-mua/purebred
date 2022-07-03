{ mkDerivation, base, bytestring, c2hs, deepseq, filepath, lib, mtl
, notmuch, profunctors, tagged, talloc, text, time
}:
mkDerivation {
  pname = "notmuch";
  version = "0.3.1";
  sha256 = "d9aa57707cc21ee9564399a901ade589793d6619ddacfcdacb81ddf725cd7dd3";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring deepseq filepath mtl profunctors tagged text time
  ];
  librarySystemDepends = [ notmuch talloc ];
  libraryToolDepends = [ c2hs ];
  homepage = "https://github.com/purebred-mua/hs-notmuch";
  description = "Haskell binding to Notmuch, the mail indexer";
  license = lib.licenses.gpl3Only;
}
