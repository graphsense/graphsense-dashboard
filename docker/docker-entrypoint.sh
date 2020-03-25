#!/bin/ash
set -e

sed -i "s|{{REST_ENDPOINT}}|$REST_ENDPOINT|g" /usr/share/nginx/html/main.js 
sed -i "s|{{TITANIUM_REPORT_GENERATION_URL}}|$TITANIUM_REPORT_GENERATION_URL|g" /usr/share/nginx/html/main.js 

exec "$@"
