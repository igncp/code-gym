#!/usr/bin/env bash

set -e

../node_modules/.bin/prettier \
  '*.ts' \
  '*.js' \
  "$@"
