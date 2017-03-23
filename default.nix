{ bootFetchgit ? (import <nixpkgs> {}).fetchgit, compiler ? (import <nixpkgs> {}).haskell.packages.ghc802 }:
let
  pkgs = import (bootFetchgit (import ./version.nix)) {};
  cabalPkg = compiler.callPackage ./package.nix {};
in cabalPkg
