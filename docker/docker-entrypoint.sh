#!/bin/bash
set -e

for plugin in `find ./plugins -mindepth 1 -maxdepth 1 -type d`; do 
    cd ./$plugin 
    npm install
    cd -
done

# theme has been built at image build time
npm run build-without-theme && cp -r ./dist/* /usr/share/nginx/html/ 

# remove node_modules to save image space
find ./plugins -name node_modules -exec rm -rf {} \; || true

chown -R $DOCKER_UID /usr/share/nginx/html/*

sed -i "s|http://localhost:9000|$REST_URL|g" /usr/share/nginx/html/assets/index*.js 

exec "$@"
