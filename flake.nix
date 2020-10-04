{
  description = "Development environment for learn4haskell";

  outputs = { self, nixpkgs }: {

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "learn4haskell";
        buildInputs = [
          haskell.compiler.ghc8102
          cabal-install
          haskellPackages.brittany
          haskellPackages.hlint
        ];
      };
  };
}
