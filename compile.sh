ghc --make -threaded site.hs;
rm -rf _site
./site clean
./site build
