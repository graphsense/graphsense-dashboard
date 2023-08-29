FROM alpine:3.16
LABEL maintainer="contact@iknaio.com"

ENV DOCKER_USER=user
ARG DOCKER_UID=1000

RUN addgroup -S $DOCKER_USER && adduser -S $DOCKER_USER -G $DOCKER_USER -u $DOCKER_UID

ENV WORKDIR=/app

RUN mkdir $WORKDIR && \
    apk --no-cache --update add bash nginx nodejs npm && \
    apk --no-cache --update --virtual build-dependendencies add python3 make g++


WORKDIR $WORKDIR
COPY ./docker/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh 

COPY ./elm.json ./elm-tooling.json ./index.html ./package*.json ./vite.config.js $WORKDIR/

COPY ./config $WORKDIR/config
COPY ./src $WORKDIR/src
COPY ./openapi $WORKDIR/openapi
COPY ./public $WORKDIR/public
COPY ./plugins $WORKDIR/plugins
COPY ./plugin_templates $WORKDIR/plugin_templates
COPY ./themes $WORKDIR/themes
COPY ./docker/site.conf /etc/nginx/http.d/
COPY ./generate.js $WORKDIR/generate.js

RUN chown -R $DOCKER_USER $WORKDIR && \
    mkdir -p /usr/share/nginx/html /run/nginx && \
    chown -R $DOCKER_USER /usr/share/nginx/html && \
    rm -f /etc/nginx/http.d/default.conf 

USER $DOCKER_USER
RUN npm install 

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["nginx", "-g", "daemon off;"]
EXPOSE 8000
