#!/usr/bin/env bash

set -e

sh ../scripts/_eslint_common.sh

sh ../scripts/_prettier_common.sh -c

../node_modules/.bin/type-coverage \
  -p . \
  --detail \
  --strict \
  --at-least 100
