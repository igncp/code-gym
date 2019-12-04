#!/usr/bin/env bash

set -e

sh ../scripts/_eslint_common.sh --fix

echo "Ran eslint fix"

sh ../scripts/_prettier_common.sh --write
