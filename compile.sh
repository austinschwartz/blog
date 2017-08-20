#!/bin/sh

sudo rm -rf _site;
sudo rm -rf _cache;

ghc --make -threaded bin/hakyll.hs 

./bin/hakyll clean
./bin/hakyll build

./bin/deploy.sh

