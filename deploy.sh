#!/bin/sh

# Build books file
ruby scripts/goodreads-scraper/build.rb > partials/books.html

# Delete cache
rm -rf _site
./site clean

# Rebuild
./site build

# Upload (commented out since I build on my server)
#rsync -avz _site/ root@austinschwartz.com:/var/www/austinschwartz.com/

# Copy to our static site directory :)
sudo cp -r _site/* /var/www/html/

