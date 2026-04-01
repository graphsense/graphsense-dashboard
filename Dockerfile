FROM node:22-alpine AS builder

ENV WORKDIR=/app

RUN mkdir ${WORKDIR} && \
    apk --no-cache --update add bash git openssh python3 make g++ rsync

WORKDIR $WORKDIR

COPY ./elm.json.base ./elm-tooling.json ./index.html ./package*.json ./vite.config.mjs ./Makefile $WORKDIR/

COPY ./config $WORKDIR/config
RUN cp -n $WORKDIR/config/Config.elm.tmp $WORKDIR/config/Config.elm
COPY ./src $WORKDIR/src
COPY ./openapi $WORKDIR/openapi
COPY ./public $WORKDIR/public
COPY ./lang $WORKDIR/lang
#COPY ./generated/theme $WORKDIR/generated/theme
COPY ./plugins $WORKDIR/plugins
COPY ./plugin_templates $WORKDIR/plugin_templates
COPY ./themes $WORKDIR/themes
COPY ./theme $WORKDIR/theme
COPY ./codegen $WORKDIR/codegen
COPY ./lib $WORKDIR/lib
COPY ./docker/site.conf /etc/nginx/http.d/
COPY ./tools/generate.js $WORKDIR/tools/generate.js

RUN mkdir -p /usr/share/nginx/html /run/nginx && \
    rm -f /etc/nginx/http.d/default.conf 

COPY ./docker/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh

COPY ./tools $WORKDIR/tools

RUN touch .env && make build

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["nginx", "-g", "pid /tmp/nginx.pid;daemon off;"]
EXPOSE 8000
