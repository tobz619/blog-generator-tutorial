{ mkDerivation, base, directory, filepath, hs-blog, hspec
, hspec-discover, lib, mtl, optparse-applicative, raw-strings-qq
}:
mkDerivation {
  pname = "my-blog-tut";
  version = "0.1.0.0";
  src = /nix/store/p73gqx66lki9zrsaz5wg8z81b1z10fwq-my-blog-tut;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath mtl ];
  executableHaskellDepends = [ base directory optparse-applicative ];
  testHaskellDepends = [
    base hs-blog hspec hspec-discover raw-strings-qq
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "tobioloke.com";
  license = lib.licenses.bsd3;
  mainProgram = "hs-blog-generator";
}
