FROM alpine:3.15
LABEL maintainer="contact@graphsense.info"

ENV WORKDIR=/app

RUN mkdir $WORKDIR && \
    apk --no-cache --update add bash nginx && \
    apk --no-cache --update --virtual build-dependendencies add npm nodejs

WORKDIR $WORKDIR
COPY ./docker/docker-entrypoint.sh /
COPY ./config $WORKDIR/config
COPY ./src $WORKDIR/src
COPY ./lang $WORKDIR/lang
COPY ./lib $WORKDIR/lib
COPY ./*.js ./*package.json $WORKDIR/
COPY ./docker/site.conf /etc/nginx/http.d/

RUN chmod +x /docker-entrypoint.sh && \
    npm install && \
    npm run build && \
    mkdir -p /usr/share/nginx/html /run/nginx && \
    mv $WORKDIR/dist/* /usr/share/nginx/html/ && \
    rm -r /root/.npm && \
    rm /etc/nginx/http.d/default.conf && \
    apk del build-dependendencies

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["nginx", "-g", "daemon off;"]
EXPOSE 8000
