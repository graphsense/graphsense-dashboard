FROM alpine:3.15
LABEL maintainer="contact@graphsense.info"

ENV WORKDIR=/app

RUN mkdir $WORKDIR && \
    apk --no-cache --update add bash nginx && \
    apk --no-cache --update --virtual build-dependendencies add npm nodejs python3 make g++

WORKDIR $WORKDIR
COPY ./docker/docker-entrypoint.sh /
COPY ./config $WORKDIR/config
COPY ./src $WORKDIR/src
COPY ./openapi $WORKDIR/openapi
COPY ./elm-hovercard $WORKDIR/elm-hovercard
COPY ./elm-css-sortable-table $WORKDIR/elm-css-sortable-table
COPY ./lang $WORKDIR/lang
COPY ./plugins $WORKDIR/plugins
COPY ./themes $WORKDIR/themes
COPY ./elm.json ./elm-tooling.json ./index.html ./package*.json ./vite.config.js $WORKDIR/
COPY ./docker/site.conf /etc/nginx/conf.d/

RUN chmod +x /docker-entrypoint.sh && \
    npm install && \
    npm run build && \
    mkdir -p /usr/share/nginx/html /run/nginx && \
    mv $WORKDIR/dist/* /usr/share/nginx/html/ && \
    rm -r /root/.config /root/.npm && \
    rm /etc/nginx/conf.d/default.conf && \
    apk del build-dependendencies

CMD ["nginx", "-g", "daemon off;"]
EXPOSE 8000
