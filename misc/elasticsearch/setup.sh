#!/bin/bash

set -e

# disable trial features
sed \
  -i 's|xpack.license.self_generated.type: .*|xpack.license.self_generated.type: basic|' \
  docker-elk/elasticsearch/config/elasticsearch.yml

echo "The setup finished correctly"
