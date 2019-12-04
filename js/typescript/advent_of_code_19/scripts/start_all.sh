#!/usr/bin/env bash

set -e

sh scripts/_get_dirs.sh | while read -r DIR_NAME; do
  (\
    cd "$DIR_NAME" \
    && echo "Starting: $DIR_NAME" \
    && npm start -s \
    && echo ""
  )
done
