docker stop dashboard
docker rm dashboard
docker run --restart=always -d --name dashboard -p 8000:8000 -it dashboard
docker ps
