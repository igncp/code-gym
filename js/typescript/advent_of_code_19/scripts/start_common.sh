#!/usr/bin/env bash

set -e

rm -rf dist

printf "Compiling..."

../node_modules/.bin/tsc --project .

printf " Compiled.\n\n"

if [ -f input.txt ]; then
  cp input.txt dist
fi

node dist/main.js
