#!/bin/sh

mkdir -p dist
ghc -o dist/HoneyComb src/HoneyComb.hs
mv src/*.o dist/
mv src/*.hi dist/
