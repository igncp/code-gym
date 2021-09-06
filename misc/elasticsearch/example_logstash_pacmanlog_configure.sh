#!/usr/bin/env bash

set -e

# This example maps to a real log file, so the data should be updated whenever
# the file is updated

# FILE: https://www.elastic.co/guide/en/logstash/current/plugins-inputs-file.html
# GROK: https://www.elastic.co/guide/en/logstash/current/plugins-filters-grok.html
# https://grokdebug.herokuapp.com/

cd docker-elk

HAS_VOLUME="$(grep pacman.log docker-compose.yml || true)"

if [ -z "$HAS_VOLUME" ]; then
  read -r -d '' NEW_VOLUME << EOM || true
- type: bind
        source: /var/log/pacman.log
        target: /usr/share/pacman.log
        read_only: true
EOM
  REF_LINE_NUMBER="$(grep -n '/usr/share/logstash/pipeline' docker-compose.yml | cut -f1 -d:)"
  NEW_LINE_NUMBER=$(($REF_LINE_NUMBER + 1)) # it has the read_only line after

  head -n"$NEW_LINE_NUMBER" docker-compose.yml > /tmp/docker-compose.yml \
    && echo "      $NEW_VOLUME" >> /tmp/docker-compose.yml \
    && tail -n +"$NEW_LINE_NUMBER" docker-compose.yml >> /tmp/docker-compose.yml \
    && mv /tmp/docker-compose.yml docker-compose.yml && rm -rf /tmp/docker-compose.yml
fi

cat > logstash/pipeline/pacmanlog.conf <<"EOF"
input {
  file {
    path => "/usr/share/pacman.log"
    start_position => "beginning"
    tags => ["pacman-log"]
  }
}

filter {
  if "pacman-log" in [tags] {
    grok {
      match => {
        "message" => "\[%{TIMESTAMP_ISO8601:date}\] \[%{DATA:entity}\] %{GREEDYDATA:action}"
      }
    }
  }
}

output {
  if "pacman-log" in [tags] {
    elasticsearch {
      ecs_compatibility => disabled
      hosts => "elasticsearch:9200"
      index => "demo-pacmanlog"
      password => "changeme"
      user => "elastic"
    }
  }
}
EOF

docker-compose restart kibana logstash

echo "Configure finished correctly"
