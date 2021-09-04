#!/bin/bash

set -e

# https://www.elastic.co/guide/en/elasticsearch/reference/current/rest-apis.html

# http://localhost:5601/app/dev_tools#/console

delete_csv_docs() {
  curl \
    -XPOST "http://localhost:9200/demo-csv/_delete_by_query" \
    -H "Content-Type: application/json" \
    --user elastic:changeme \
    -d'
{
  "query": {
    "match_all": {}
  }
}' | jq
}

get_logstash_index_config() {
  curl \
    -XGET "http://localhost:9200/logstash" \
    -H "Content-Type: application/json" \
    --user elastic:changeme | jq
}

get_logstash_pipeline() {
  curl \
    -XGET "http://localhost:9200/_logstash/pipeline" \
    -H "Content-Type: application/json" \
    --user elastic:changeme | jq
}

get_search_all() {
  curl \
    -XGET "http://localhost:9200/_search" \
    -H "Content-Type: application/json" \
    --user elastic:changeme \
    -d'
{
  "query": {
    "match_all": {}
  }
}' | jq
}

delete_csv_docs
