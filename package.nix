{ mkDerivation, base, criterion, doctest, doctest-discover
, ghc-prim, hspec, stdenv
}:
mkDerivation {
  pname = "katas";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base criterion doctest ghc-prim hspec ];
  executableHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base doctest doctest-discover hspec ];
  homepage = "https://github.com/mbbx6spp/katas";
  description = "Exploration of functional programming in Haskell using Katas to demonstrate ideas and concepts";
  license = stdenv.lib.licenses.bsd3;
}
