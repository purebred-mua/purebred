{ mkDerivation, base, binary, directory, executable-path, fetchgit
, filepath, io-storage, lib, process, time, unix, xdg-basedir
}:
mkDerivation {
  pname = "dyre";
  version = "0.9.1";
  src = fetchgit {
    url = "https://github.com/willdonnelly/dyre.git";
    sha256 = "1ldxl54gj0zy3vmqiiyikxcy5yyanxhm789bagkavizs5fhd9gj2";
    rev = "f1ebc1592fb188fa173f8c1baa325fc61f527825";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base binary directory executable-path filepath io-storage process
    time unix xdg-basedir
  ];
  testHaskellDepends = [ base directory process ];
  doCheck = false;
  homepage = "http://github.com/willdonnelly/dyre";
  description = "Dynamic reconfiguration in Haskell";
  license = lib.licenses.bsd3;
}
