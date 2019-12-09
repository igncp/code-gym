#!/usr/bin/env bash

set -e

sh ../scripts/_compile_common.sh

if [ -f input.txt ]; then
  cp input.txt dist
fi

node dist/main.js
