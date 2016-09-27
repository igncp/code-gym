#!/usr/bin/env bash

run_and_echo() {
  CMD="$1;"
  echo "$CMD"
  neo4j-shell -c "$CMD"
  echo ""
  echo ""
}

get_random_existing_node_id() {
  NODE_ID=$(run_and_echo "MATCH (a) RETURN a, rand() as r ORDER BY r LIMIT 1" \
    | grep "Node" | grep -o -E "\[.+\]" | sed -r "s|\[(.+)\]|\1|")
  echo "$NODE_ID"
}

RANDOM_NODE_WORDS=$(shuf -n 3 ~/english-words/words2.txt)
RANDOM_NODE_WORDS=($RANDOM_NODE_WORDS)

for RANDOM_WORD in "${RANDOM_NODE_WORDS[@]}"; do
  TIMES=$(((RANDOM % 3) + 1))
  for _ in $(seq 1 $TIMES); do
    run_and_echo "CREATE (n:$RANDOM_WORD)"
  done
done

RANDOM_RELATIONSHIP_WORD=$(shuf -n 1 ~/english-words/words2.txt)

for _ in $(seq 1 4); do
  NODE_ID_A=$(get_random_existing_node_id)
  NODE_ID_B=$(get_random_existing_node_id)
  run_and_echo "MATCH (n), (m)
  WHERE ID(n)=$NODE_ID_A and ID(m)=$NODE_ID_B
  CREATE (n)-[:$RANDOM_RELATIONSHIP_WORD]->(m)"
done
