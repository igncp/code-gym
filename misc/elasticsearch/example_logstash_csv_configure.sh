#!/usr/bin/env bash

set -e

# https://www.elastic.co/guide/en/logstash/current/plugins-filters-csv.html
# https://www.elastic.co/guide/en/logstash/current/config-examples.html
# https://github.com/logstash-plugins/logstash-filter-csv

cd docker-elk

HAS_VOLUME="$(grep file.csv docker-compose.yml || true)"

if [ -z "$HAS_VOLUME" ]; then
  read -r -d '' NEW_VOLUME << EOM || true
- type: bind
        source: ./logstash/file.csv
        target: /usr/share/file.csv
        read_only: true
EOM
  REF_LINE_NUMBER="$(grep -n '/usr/share/logstash/pipeline' docker-compose.yml | cut -f1 -d:)"
  NEW_LINE_NUMBER=$(($REF_LINE_NUMBER + 1)) # it has the read_only line after

  head -n"$NEW_LINE_NUMBER" docker-compose.yml > /tmp/docker-compose.yml \
    && echo "      $NEW_VOLUME" >> /tmp/docker-compose.yml \
    && tail -n +"$NEW_LINE_NUMBER" docker-compose.yml >> /tmp/docker-compose.yml \
    && mv /tmp/docker-compose.yml docker-compose.yml && rm -rf /tmp/docker-compose.yml
fi

if [ ! -f logstash/file.csv ]; then
  echo "file.csv missing, copy it"
  exit 1
fi

cat > logstash/pipeline/logstash.conf <<"EOF"
input {
  beats {
    port => 5044
  }

  file {
    path => "/usr/share/file.csv"
    start_position => "beginning"
  }

  tcp {
    port => 5000
  }
}

filter {
  csv {
    separator => ","
    autodetect_column_names => true
    skip_header => false
    convert => {
      "Year of Release" => "integer"
    }
  }
}

output {
  elasticsearch {
    ecs_compatibility => disabled
    hosts => "elasticsearch:9200"
    index => "demo-csv"
    password => "changeme"
    user => "elastic"
  }
}
EOF

docker-compose restart kibana logstash

echo "Configure finished correctly"
