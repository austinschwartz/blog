#!/bin/sh
# Taken from https://github.com/zacharydenton/zach.se
find _site -name "*.html" -print0 | xargs -0 bin/katex.js
