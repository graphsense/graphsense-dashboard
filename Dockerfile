FROM debian:bookworm-slim
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
    apt update && \
    apt install -y bash nginx nodejs npm python3 make g++ jq && \
    rm -rf /var/lib/apt/lists/*


WORKDIR $WORKDIR

COPY ./elm.json.base ./elm-tooling.json ./index.html ./package*.json ./vite.config.mjs ./Makefile $WORKDIR/

COPY ./config $WORKDIR/config
RUN cp $WORKDIR/config/Config.elm.tmp $WORKDIR/config/Config.elm
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
COPY ./generate.js $WORKDIR/generate.js

RUN mkdir -p /usr/share/nginx/html /run/nginx && \
    rm -f /etc/nginx/http.d/default.conf 

COPY ./docker/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh

COPY ./tools $WORKDIR/tools

#RUN touch .env && make prepare

#ENTRYPOINT ["/docker-entrypoint.sh"]
#CMD ["nginx", "-g", "daemon off;"]
#EXPOSE 8000
