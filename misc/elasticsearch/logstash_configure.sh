#!/usr/bin/env bash

set -e

# https://www.elastic.co/guide/en/logstash/current/config-examples.html

cd docker-elk

# @TODO:
# https://www.bmc.com/blogs/elasticsearch-load-csv-logstash/

cat > logstash/pipeline/logstash.conf <<"EOF"
input {
	beats {
		port => 5044
	}

	tcp {
		port => 5000
	}
}


output {
	elasticsearch {
		hosts => "elasticsearch:9200"
		user => "elastic"
		password => "changeme"
		ecs_compatibility => disabled
	}
}
EOF

docker-compose restart kibana logstash

echo "Configure finished correctly"
