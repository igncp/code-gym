#!/usr/bin/env bash

CMD="MATCH (n) DETACH DELETE n;"
echo "$CMD"
neo4j-shell -c "$CMD"
