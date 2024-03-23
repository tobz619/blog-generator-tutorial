{ pkgs ? import <nixpkgs> {}
}:

let
  pname = builtins.baseNameOf (builtins.toString ./.);

in 

  pkgs.haskell.packages.ghc948.shellFor {
    packages = hpkgs: [
      hpkgs.distribution-nixpkgs (hpkgs.callPackage ./${pname}.nix {})
    ];

    nativeBuildInputs = with pkgs; [
      cabal-install
      cabal2nix
      haskell-language-server
    ];

    distrution_nixpkgs_datadir = toString ./distribution-nixpkgs;
  }


