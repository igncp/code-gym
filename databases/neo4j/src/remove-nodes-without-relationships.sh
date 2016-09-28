#!/usr/bin/env bash

QUERY="MATCH (a) WHERE NOT (a)-[]-() DELETE a;"

echo "$QUERY"

neo4j-shell -c "$QUERY"
