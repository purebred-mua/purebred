{ mkDerivation, async, base, base64-bytestring, bytestring, hspec
, lib, process, stm, temporary, transformers, unliftio-core
}:
mkDerivation {
  pname = "typed-process";
  version = "0.2.8.0";
  sha256 = "8578da545d6b2fa4b0b7296be389a736739153ced19d1dffbdee68aec978c0a9";
  revision = "1";
  editedCabalFile = "1m017nqbaqishii32gwhxa1849h0qnn06w7k1rn8c9d8w71m4vqm";
  libraryHaskellDepends = [
    async base bytestring process stm transformers unliftio-core
  ];
  testHaskellDepends = [
    async base base64-bytestring bytestring hspec process stm temporary
    transformers unliftio-core
  ];
  homepage = "https://github.com/fpco/typed-process";
  description = "Run external processes, with strong typing of streams";
  license = lib.licenses.mit;
}
