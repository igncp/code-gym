#!/usr/bin/env bash

run_and_echo() {
  CMD="$1;"
  echo "$CMD"
  neo4j-shell -c "$CMD"
  echo ""
  echo ""
}

run_and_echo "MATCH (n) RETURN COUNT(n)"

run_and_echo "MATCH (n)-[r]-(m) RETURN COUNT(r)"

run_and_echo "MATCH (n) RETURN DISTINCT LABELS(n)"
