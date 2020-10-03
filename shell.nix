{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    haskell.compiler.ghc8102
    cabal-install
    haskellPackages.brittany
    haskellPackages.hlint
  ];
}
