#!/usr/bin/env bash

set -e

curl ${APISIX_URL:-http://localhost:9180}/apisix/admin/routes/ws_test \
  -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" \
  -X PUT -d '
{
  "name": "Testing route",
  "enable_websocket": true,
  "uri": "/ws",
  "plugins": {
    "myplugin": {
      "path": "foo"
    }
  },
  "upstream": {
    "type": "roundrobin",
    "nodes": {
      "websocker-echo:8080": 1
    }
  }
}' | jq
