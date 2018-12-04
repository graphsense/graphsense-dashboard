#!/bin/bash
set -e

v=`printf "%q" "$REST_ENDPOINT"`
sed -i "s|{{REST_ENDPOINT}}|$v|" /usr/share/nginx/html/bundle.js 

exec "$@"
