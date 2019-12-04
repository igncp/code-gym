#!/usr/bin/env bash

set -e

../node_modules/.bin/eslint . --ext ts "$@"
