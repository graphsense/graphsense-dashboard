#!/bin/ash
set -e

echo $DOCKER_UID
echo $DOCKER_USER
addgroup $DOCKER_USER 
adduser $DOCKER_USER -G $DOCKER_USER -u $DOCKER_UID -D

chown -R $DOCKER_USER $WORKDIR
chown -R $DOCKER_USER /usr/share/nginx/html
chown -R $DOCKER_USER /var/lib/nginx 
chown -R $DOCKER_USER /var/log/nginx

su $DOCKER_USER

exec "$@"
