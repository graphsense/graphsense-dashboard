FROM node:alpine AS builder

ENV WORKDIR=/app

RUN mkdir $WORKDIR
WORKDIR $WORKDIR
# clone from a master branch at a given version instead:
COPY . $WORKDIR
RUN npm install && npm run build

FROM nginx:alpine
COPY --from=builder /app/dist /usr/share/nginx/html
COPY ./docker/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT [ "/docker-entrypoint.sh" ]
CMD ["nginx", "-g", "daemon off;"]

