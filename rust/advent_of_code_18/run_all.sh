#!/usr/bin/env bash

set -e

DIRS=$(find . -maxdepth 1 -mindepth 1 -type d | sort -V)

while read -r DIR; do
  printf "\n\n$(basename "$DIR")\n\n"
  (cd "$DIR" && cargo run --quiet)
done <<< "$DIRS"
