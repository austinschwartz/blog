#!/bin/sh

ghc --make -threaded site.hs;
rm -rf _site
rm -rf _cache
./site clean
./site build
