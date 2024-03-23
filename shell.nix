{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/fa9f817df522ac294016af3d40ccff82f5fd3a63.tar.gz") {}
, shell-dir ? ./. 
}:

let
  pname = builtins.baseNameOf (builtins.toString shell-dir);

in

pkgs.haskell.packages.ghc948.shellFor {
  packages = hpkgs: [
    hpkgs.distribution-nixpkgs
    ( hpkgs.callPackage (shell-dir + "/${pname}.nix") {} )
  ];

  nativeBuildInputs = with pkgs; [
    cabal-install
    haskell.packages.ghc928.haskell-language-server
    cabal2nix
    stack
  ];

  shellHook = '' 
    echo "... updating ${pname}.nix ..."
    cabal2nix ${shell-dir} > ${pname}.nix  
  '';
  
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
