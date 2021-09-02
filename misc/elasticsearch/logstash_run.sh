#!/bin/bash

set -e

list_plugins() {
  docker-compose run logstash \
    bin/logstash-plugin list \
      --verbose
}

cd docker-elk

list_plugins
