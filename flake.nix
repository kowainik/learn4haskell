{
  description = "Development environment for learn4haskell";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        name = "learn4haskell";
        devShell = import ./shell.nix { inherit pkgs; };
      });
}
