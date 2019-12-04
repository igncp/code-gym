#!/usr/bin/env bash

set -e

(\
  cd scripts \
  && sh _prettier.sh \
  '../.eslintrc.js' \
  '../jest.config.js' \
  -c
)

echo ""

sh scripts/_get_dirs.sh | while read -r DIR_NAME; do
  (\
    cd "$DIR_NAME" \
    && echo "Checking: $DIR_NAME" \
    && npm run check -s \
    && echo ""
  )
done
