{ mkDerivation, base, lib }:
mkDerivation {
  pname = "my-blog-tut";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  homepage = "tobioloke.com";
  license = lib.licenses.gpl3Only;
  mainProgram = "my-blog-tut";
}
