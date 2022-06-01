#!/bin/ash
set -e

sed -i "s|http://localhost:9000|$REST_URL|g" /usr/share/nginx/html/assets/index.*.js 

exec "$@"
