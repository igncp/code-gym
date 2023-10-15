#!/usr/bin/env bash

set -e

# curl "${APISIX_URL:-http://localhost:9180}/apisix/admin/routes" \
#   -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" | jq

# curl "${APISIX_URL:-http://localhost:9180}/apisix/admin/plugins/myplugin" \
#   -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" | jq

# # Deprecated
# curl "${APISIX_URL:-http://localhost:9180}/apisix/admin/plugins?all=true" \
#   -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" | jq

# curl "${APISIX_URL:-http://localhost:9180}/apisix/admin/plugins/syslog" \
#   -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" | jq

# curl "${APISIX_URL:-http://localhost:9180}/apisix/admin/plugins/reload" \
#   -X PUT -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}"

# curl "${APISIX_URL:-http://localhost:9180}/apisix/admin/upstreams" \
# -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" | jq

# curl "${APISIX_URL:-http://localhost:9180}/apisix/admin/routes/route_test" \
#   -X DELETE \
#   -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" | jq

# curl ${APISIX_URL:-http://localhost:9180}/apisix/admin/routes/route_test \
#   -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" \
#   -X PUT -d '
# {
#   "name": "Testing route",
#   "methods": [
#     "GET"
#   ],
#   "uri": "/foo",
#   "plugins": {
#     "proxy-rewrite": {
#       "uri": "/ip"
#     },
#     "myplugin": {
#       "path": "logs/file.log"
#     }
#   },
#   "upstream": {
#     "type": "roundrobin",
#     "nodes": {
#       "httpbin.org:80/ip": 1
#     }
#   }
# }' | jq

# curl http://localhost:9080/foo

# curl ${APISIX_URL:-http://localhost:9180}/apisix/admin/routes/ws_test \
#   -H "X-API-KEY: ${APISIX_KEY:-edd1c9f034335f136f87ad84b625c8f1}" \
#   -X PUT -d '
# {
#   "name": "Testing route",
#   "enable_websocket": true,
#   "methods": ["GET"],
#   "uri": "/*",
#   "upstream": {
#     "type": "roundrobin",
#     "nodes": {
#       "127.0.0.1:8080": 1
#     }
#   }
# }' | jq
