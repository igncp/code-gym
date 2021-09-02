#!/bin/bash

set -e

get_root_info() {
  curl \
    -XGET 'localhost:9600/?pretty' |
    jq
}

get_pipelines_info() {
  curl \
    -XGET 'localhost:9600/_node/pipelines' |
    jq
}

get_pipelines_stats() {
  curl \
    -XGET 'localhost:9600/_node/stats/pipelines' |
    jq
}

get_os_info() {
  curl \
    -XGET 'localhost:9600/_node/os' |
    jq
}

get_plugins_info() {
  curl \
    -XGET 'localhost:9600/_node/plugins' |
    jq
}

# get_root_info
# get_os_info
# get_pipelines_info
# get_plugins_info

get_pipelines_stats
