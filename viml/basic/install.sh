#!/usr/bin/env bash

DIRS=$(find . -maxdepth 1 -mindepth 1 -type d)

while read -r DIR; do
  DIR_NAME=$(basename "$DIR")
  rm -rf ~/.vim/bundle/"$DIR_NAME"
  ln -s $(pwd)/"$DIR_NAME" ~/.vim/bundle
done <<< "$DIRS"
