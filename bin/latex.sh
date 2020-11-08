#!/bin/sh
# Taken from https://github.com/zacharydenton/zach.se
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
nvm use 10.16.0
find _site -name "*.html" -print0 | xargs -0 bin/katex.js
