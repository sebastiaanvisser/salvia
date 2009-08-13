clear
./Setup.lhs configure &&
./Setup.lhs build &&
sudo ./Setup.lhs install --global 
sudo ./dist/build/salvia-demo/salvia-demo +RTS -N2
