FROM alpine:3.16
LABEL maintainer="contact@iknaio.com"

ENV WORKDIR=/app

RUN mkdir $WORKDIR && \
    apk --no-cache --update add bash nginx && \
    apk --no-cache --update --virtual build-dependendencies add npm nodejs python3 make g++

WORKDIR $WORKDIR
COPY ./docker/docker-entrypoint.sh /
COPY ./elm.json ./elm-tooling.json ./index.html ./package*.json ./vite.config.js $WORKDIR/

RUN npm install 

RUN chmod +x /docker-entrypoint.sh 

COPY ./config $WORKDIR/config
COPY ./src $WORKDIR/src
COPY ./openapi $WORKDIR/openapi
COPY ./public $WORKDIR/public
COPY ./plugins $WORKDIR/plugins
COPY ./plugin_templates $WORKDIR/plugin_templates
COPY ./themes $WORKDIR/themes
COPY ./docker/site.conf /etc/nginx/http.d/
COPY ./generate.js $WORKDIR/generate.js

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["nginx", "-g", "daemon off;"]
EXPOSE 8000
