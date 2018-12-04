FROM node:alpine AS builder

ENV WORKDIR=/app

RUN mkdir $WORKDIR
WORKDIR $WORKDIR
# clone from a master branch at a given version instead:
COPY ./package.json $WORKDIR
RUN npm install
COPY ./src $WORKDIR/src
COPY ./webpack.config.js $WORKDIR
COPY ./postcss.config.js $WORKDIR
COPY ./tailwind.js $WORKDIR
COPY ./babel.config.js $WORKDIR
RUN npm run build

FROM nginx:alpine
COPY --from=builder /app/dist /usr/share/nginx/html
COPY ./docker/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT [ "/docker-entrypoint.sh" ]
CMD ["nginx", "-g", "daemon off;"]

