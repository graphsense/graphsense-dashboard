#!/bin/ash
set -e

sed -i "s|http://graphsense.rest.local:9000|$REST_URL|g" /usr/share/nginx/html/assets/index.*.js 

exec "$@"
