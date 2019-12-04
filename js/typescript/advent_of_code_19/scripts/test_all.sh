#!/usr/bin/env bash

set -e

sh scripts/_get_dirs.sh | while read -r DIR_NAME; do
  (\
    cd "$DIR_NAME" \
    && echo "Testing: $DIR_NAME" \
    && npm test -s -- --coverage \
    && echo ""
  )
done
