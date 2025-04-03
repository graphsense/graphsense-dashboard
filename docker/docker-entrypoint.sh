#!/bin/bash
set -e

make build
cp -r ./dist/* /usr/share/nginx/html/ 

# remove node_modules to save space
find ./plugins -maxdepth 2 -name node_modules -exec rm -rf {} \; || true

chown -R $DOCKER_UID /usr/share/nginx/html/*

sed -i "s|http://localhost:9000|$REST_URL|g" /usr/share/nginx/html/assets/index*.js 

exec "$@"
