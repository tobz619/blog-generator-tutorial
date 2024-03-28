{ mkDerivation, base, directory, lib, optparse-applicative }:
mkDerivation {
  pname = "my-blog-tut";
  version = "0.1.0.0";
  src = /nix/store/vaig7lzhs1rp3svyjxgw40h4vd2j6y7n-my-blog-tut;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory ];
  executableHaskellDepends = [ base directory optparse-applicative ];
  homepage = "tobioloke.com";
  license = lib.licenses.bsd3;
  mainProgram = "hs-blog-generator";
}
