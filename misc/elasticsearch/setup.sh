#!/bin/bash

set -e

cd docker-elk

# disable trial features
sed \
  -i 's|xpack.license.self_generated.type: .*|xpack.license.self_generated.type: basic|' \
  elasticsearch/config/elasticsearch.yml

HAS_PIPELINES=$(grep pipeline.workers ./logstash/config/logstash.yml || true)
if [ -z "$HAS_PIPELINES" ]; then
  echo 'pipeline.workers: 1' >> ./logstash/config/logstash.yml
fi

sed \
  -i 's|ES_JAVA_OPTS:.*|ES_JAVA_OPTS: "-Xms2g -Xmx2g"|' \
  docker-compose.yml

echo "The setup finished correctly"
