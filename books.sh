#!/bin/sh

# Build books file
ruby scripts/goodreads-scraper/build.rb > partials/books.html
