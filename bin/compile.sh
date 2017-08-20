#!/bin/sh
sudo tee /etc/sudoers.d/$USER <<END
END

ghc --make -threaded site.hs;
rm -rf ../_site
rm -rf ../_cache

./site clean
./site build
