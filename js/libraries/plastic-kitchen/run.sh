#!/usr/bin/env bash

sudo docker build \
  -t puppeteer-testing \
  .

sudo docker run \
  --rm \
  -v "$(pwd):/code" \
  --name puppeteer-testing \
  puppeteer-testing
