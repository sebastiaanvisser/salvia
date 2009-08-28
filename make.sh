clear
cabal configure &&
cabal build &&
cabal install &&
./dist/build/salvia-demo/salvia-demo +RTS -N2

