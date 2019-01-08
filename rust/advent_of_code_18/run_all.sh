#!/usr/bin/env bash

set -e

DIRS=$(find . -maxdepth 1 -mindepth 1 -type d | sort -V)

# TODO: add this line when ready
# cargo clippy --all-targets --all-features --quiet -- -D warnings && \

while read -r DIR; do
  NAME=$(basename "$DIR")
  printf "\n\n$NAME\n\n"
  (cd "$DIR" && \
    printf "Building $NAME..." && \
    cargo build --release --quiet && \
    printf " Built.\n"
    cargo test --quiet --release && \
    cargo run --quiet --release)
done <<< "$DIRS"
