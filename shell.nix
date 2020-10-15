{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    gnumake
    ghc
    haskellPackages.cabal-install

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
