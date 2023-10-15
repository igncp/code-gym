#!/usr/bin/env bash

set -e

# Consider running:
# bash ./scripts/print_default_config.sh | bat -l yaml
# bash ./scripts/print_default_config.sh | grep -vE '^\s*#'

if [ -z "$(docker compose ps | grep apisix)" ]; then
  echo "Please start the apisix container first."
  exit 1
fi

docker compose exec \
  apisix \
  cat conf/config-default.yaml
