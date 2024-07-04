FROM alpine:3.19
LABEL org.opencontainers.image.title="graphsense-dashboard"
LABEL org.opencontainers.image.maintainer="contact@ikna.io"
LABEL org.opencontainers.image.url="https://www.ikna.io/"
LABEL org.opencontainers.image.description="GraphSense's Web GUI for interactive cryptocurrency analysis written"
LABEL org.opencontainers.image.source="https://github.com/graphsense/graphsense-dashboard"

ENV DOCKER_USER=dockeruser
ENV DOCKER_UID=1000
ENV REST_URL=http://localhost:9000

#RUN addgroup -S $DOCKER_USER && adduser -S $DOCKER_USER -G $DOCKER_USER -u $DOCKER_UID

ENV WORKDIR=/app

RUN mkdir $WORKDIR && \
    apk --no-cache --update add bash nginx nodejs npm && \
    apk --no-cache --update --virtual build-dependendencies add python3 make g++


WORKDIR $WORKDIR

COPY ./elm.json.base ./elm-tooling.json ./index.html ./package*.json ./vite.config.js ./Makefile $WORKDIR/

COPY ./config $WORKDIR/config
RUN cp $WORKDIR/config/Config.elm.tmp $WORKDIR/config/Config.elm
COPY ./src $WORKDIR/src
COPY ./openapi $WORKDIR/openapi
COPY ./public $WORKDIR/public
COPY ./lang $WORKDIR/lang
COPY ./plugins $WORKDIR/plugins
COPY ./plugin_templates $WORKDIR/plugin_templates
COPY ./themes $WORKDIR/themes
COPY ./theme $WORKDIR/theme
COPY ./lib $WORKDIR/lib
COPY ./docker/site.conf /etc/nginx/http.d/
COPY ./generate.js $WORKDIR/generate.js

RUN mkdir -p /usr/share/nginx/html /run/nginx && \
    rm -f /etc/nginx/http.d/default.conf 

RUN npm install 

COPY ./docker/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh 

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["nginx", "-g", "pid /tmp/nginx.pid;daemon off;"]
EXPOSE 8000
