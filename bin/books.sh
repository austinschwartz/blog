#!/bin/sh

# Build books file
ruby scripts/goodreads-scraper/build.rb > partials/books.html
cat partials/books.html
git add partials/books.html
git commit -m "Updating books file"

