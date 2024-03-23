{ mkDerivation, base, directory, lib }:
mkDerivation {
  pname = "my-blog-tut";
  version = "0.1.0.0";
  src = /nix/store/z1z7yy3nn6942ilwqmx0mhlr02m1mjlk-my-blog-tut;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory ];
  executableHaskellDepends = [ base ];
  homepage = "tobioloke.com";
  license = lib.licenses.bsd3Only;
  mainProgram = "my-blog-tut";
}
