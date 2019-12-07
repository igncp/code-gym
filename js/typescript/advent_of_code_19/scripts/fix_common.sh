#!/usr/bin/env bash

set -e

sh ../scripts/_prettier_common.sh --write

sh ../scripts/_eslint_common.sh --fix
