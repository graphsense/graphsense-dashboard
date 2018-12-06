docker stop graphsense-dashboard
docker rm graphsense-dashboard
docker run --restart=always -d --name graphsense-dashboard -p 8000:8000 -it graphsense-dashboard
docker ps
