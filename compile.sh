#!/bin/sh

. ~/.nvm/nvm.sh
nvm use 10.16.0

sudo rm -rf _site;
sudo rm -rf _cache;

ghc --make -threaded bin/hakyll.hs 

echo "Hakyll build"
./bin/hakyll clean
./bin/hakyll build

echo "Latex + Highlighting"
./bin/latex.sh

echo "Goodreads scraping"
./scripts/goodreads-scraper/scrape.sh
