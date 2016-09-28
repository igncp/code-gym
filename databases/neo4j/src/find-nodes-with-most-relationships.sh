#!/usr/bin/env bash

QUERY="
MATCH (n)-[r]->(x)
RETURN n, COUNT(r), labels(n)
ORDER BY COUNT(r) DESC
LIMIT 10;
"

echo "$QUERY"

neo4j-shell -c "$QUERY"
