{ mkDerivation, base, deepseq, hspec, lib, QuickCheck, text, vector
}:
mkDerivation {
  pname = "text-zipper";
  version = "0.12";
  sha256 = "86aba7244c9ed0d8e24e9d1fa64ee317a062e7bd777018053517daefb0696702";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base deepseq text vector ];
  testHaskellDepends = [ base hspec QuickCheck text ];
  homepage = "https://github.com/jtdaugherty/text-zipper/";
  description = "A text editor zipper library";
  license = lib.licenses.bsd3;
}
