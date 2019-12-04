#!/usr/bin/env bash

set -e

(\
  cd scripts \
  && sh _prettier_common.sh \
  '../.eslintrc.js' \
  '../jest.config.js' \
  --write
)

echo ""

sh scripts/_get_dirs.sh | while read -r DIR_NAME; do
  (\
    cd "$DIR_NAME" \
    && echo "Fixing: $DIR_NAME" \
    && npm run fix -s \
    && echo ""
  )
done
