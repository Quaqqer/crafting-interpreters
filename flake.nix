{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [
            haskell.packages.ghc925.haskell-language-server
            # (haskell-language-server.override {
            #   supportedGhcVersions = [ "925" ];
            # })
            stack
            ormolu
            hlint
          ];
        };
      });
}
