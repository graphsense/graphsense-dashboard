FROM node:alpine AS builder

ENV WORKDIR=/app

RUN mkdir $WORKDIR
# clone from a master branch at a given version instead:
COPY ./src $WORKDIR/src
COPY ./webpack.config.js $WORKDIR
COPY ./package.json $WORKDIR
COPY ./postcss.config.js $WORKDIR
COPY ./tailwind.js $WORKDIR
COPY ./babel.config.js $WORKDIR
WORKDIR $WORKDIR
RUN npm install
RUN npm run build

FROM nginx
COPY --from=builder /app/dist /usr/share/nginx/html
COPY ./docker/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT [ "/docker-entrypoint.sh" ]
CMD ["nginx", "-g", "daemon off;"]

