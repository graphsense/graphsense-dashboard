FROM alpine:3.8.4

ENV WORKDIR=/app

RUN mkdir $WORKDIR && \
    apk --no-cache --update add bash nginx && \
    apk --no-cache --update --virtual build-dependendencies add npm nodejs

WORKDIR $WORKDIR
COPY ./docker/docker-entrypoint.sh /
COPY ./config $WORKDIR/config
COPY ./src $WORKDIR/src
COPY ./*.js ./*package.json $WORKDIR/
COPY ./docker/site.conf /etc/nginx/conf.d/

RUN chmod +x /docker-entrypoint.sh && \
    npm install && \
    npm run build && \
    mkdir -p /usr/share/nginx/html /run/nginx && \
    mv $WORKDIR/dist/* /usr/share/nginx/html/ && \
    rm -r /root/.config /root/.npm && \
    rm /etc/nginx/conf.d/default.conf && \
    apk del build-dependendencies

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["nginx", "-g", "daemon off;"]
EXPOSE 8000
