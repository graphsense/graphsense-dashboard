#!/bin/ash
set -e

sed -i "s|{{REST_ENDPOINT}}|$REST_ENDPOINT|" /usr/share/nginx/html/main.js 

exec "$@"
