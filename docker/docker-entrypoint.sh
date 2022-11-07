#!/bin/ash
set -e

node generate.js

for plugin in `find ./plugins -mindepth 1 -maxdepth 1 -type d`; do 
    cd $WORKDIR/$plugin 
    npm install
    cd -
    deps=$WORKDIR/$plugin/dependencies.txt
    [ ! -e "$deps" ] && continue

    for dep in `cat "$deps"`; do
        yes | ./node_modules/.bin/elm install $dep
    done
done

npm run build && \
    mkdir -p /usr/share/nginx/html /run/nginx && \
    mv $WORKDIR/dist/* /usr/share/nginx/html/ && \
    rm -rf /root/.config /root/.npm && \
    rm -f /etc/nginx/http.d/default.conf && \
    apk del build-dependendencies


sed -i "s|http://localhost:9000|$REST_URL|g" /usr/share/nginx/html/assets/index.*.js 

exec "$@"
