# GraphSense Dashboard

A Web dashboard for interactive cryptocurrency analysis.

`graphsense-dashboard` provides a Docker container running a [Flask][flask]
web application, which is deployed through uWSGI and [nginx][nginx].

## Prerequisites

Make sure [graphsense-REST][graphsense-REST] is installed and running
on your system (port 9000).

Test

    http://localhost:9000/block/1000

### Development Setup

Make sure Python 3.x is available on your system. Install the module
dependencies, e.g. via `pip`

    pip install -r requirements.txt

To start the dashboard in development mode use

    export FLASK_APP=dashboard.py
    flask run

Open the dashboard in a web browser at http://localhost:5000

### Deployment with Docker

A Docker image is provided, to deploy the web app via [nginx][nginx] and uWSGI.

Install [Docker][docker], e.g. on Debian/Ubuntu based systems

    sudo apt install docker.io

On Linux the IP address of the docker bridge network has to be 172.17.0.1:

    > ip addr show docker0
    docker0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:b7:68:a5:fb brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.1/16 scope global docker0
    ...

otherwise the settings in `conf/application.conf`

    play.filters.hosts {
      # allow requests to docker bridge
      allowed = ["172.17.0.1:9000"]
    }

in the [graphsense-REST][graphsense-REST] component needs to be adjusted.

On a Mac you have to replace this line in the `Dockerfile`

    RUN sed -ie 's/localhost/172.17.0.1/g' /srv/graphsense-dashboard/dashboard.py

to

    RUN sed -ie 's/localhost/docker.for.mac.localhost/g' /srv/graphsense-dashboard/dashboard.py


Building the docker container:

    ./docker/build.sh

Starting the container:

    ./docker/start.sh

Attaching to the container:

    ./docker/attach.sh

To view the dashboard open a web browser at http://localhost:8000


[docker]: https://www.docker.com/
[flask]: http://flask.pocoo.org/
[nginx]: https://nginx.org/en/
[graphsense-REST]: https://github.com/graphsense/graphsense-REST
