#!/usr/bin/env bash

# http://www.elasticsearchtutorial.com/elasticsearch-in-5-minutes.html
# http://joelabrahamsson.com/elasticsearch-101/

RESULT_FILE=/tmp/result

curlGetAndLog() {
  URL="http://localhost:9200/$1"
  echo "curl -XGET $URL" >> $RESULT_FILE
  echo "+++" >> $RESULT_FILE
  curl -XGET "$URL" >> $RESULT_FILE 2>/dev/null
  echo "+++" >> $RESULT_FILE
  printf "\n\n" >> $RESULT_FILE
}

curlPostAndLog() {
  URL="http://localhost:9200/$1"
  echo "curl -XPOST $URL -d '$2'" >> $RESULT_FILE
  echo "+++" >> $RESULT_FILE
  curl -XPOST "$URL" -d "$2" >> $RESULT_FILE 2>/dev/null
  printf "\n+++" >> $RESULT_FILE
  printf "\n\n" >> $RESULT_FILE
}

echo "" > $RESULT_FILE
curlGetAndLog 'blog/user/dilbert?pretty=true'
curlGetAndLog 'blog/post/1?pretty=true'
curlGetAndLog 'blog/post/2?format=yaml'
curlGetAndLog 'blog/post/3?pretty=true'

curlPostAndLog "blog/post/_search" '
{
  "query": {
    "query_string": {
      "query": "easy",
      "fields": ["body"]
    }
  }
}'

less $RESULT_FILE
