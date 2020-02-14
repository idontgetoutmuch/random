with import <nixpkgs> {};

mkShell {
  buildInputs = [
    cabal-install
    haskell.compiler.ghc822
  ];
}
