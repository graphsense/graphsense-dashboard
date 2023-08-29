#!/bin/ash
set -e

npm run gen

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

npm run build && cp -r $WORKDIR/dist/* /usr/share/nginx/html/ 

sed -i "s|http://localhost:9000|$REST_URL|g" /usr/share/nginx/html/assets/index.*.js 

exec "$@"
