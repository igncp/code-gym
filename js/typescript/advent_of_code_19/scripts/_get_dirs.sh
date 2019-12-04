#!/usr/bin/env bash

set -e

find . -maxdepth 1 -mindepth 1 -type d |
  grep -v scripts |
  grep -v node_modules |
  sort -V
