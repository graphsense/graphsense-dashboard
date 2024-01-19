#!/bin/ash
set -e

for plugin in `find ./plugins -mindepth 1 -maxdepth 1 -type d`; do 
    cd $WORKDIR/$plugin 
    npm install
    cd -
done

npm run build && cp -r $WORKDIR/dist/* /usr/share/nginx/html/ 

chown -R $DOCKER_UID /usr/share/nginx/html/*

sed -i "s|http://localhost:9000|$REST_URL|g" /usr/share/nginx/html/assets/index.*.js 

exec "$@"
