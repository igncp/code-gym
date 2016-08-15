#!/usr/bin/env bash

echo "- go to http://localhost:9000"
echo "- paste the following url: http://localhost:9200"

http-server -p9000 -c-1 -s /home/vagrant/elastic-HQ
