{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = with pkgs;
    let hs = haskell.packages.ghc928;
    in [ hs.haskell-language-server hs.cabal-install ];
}
