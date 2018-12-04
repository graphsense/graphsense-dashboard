# GraphSense GUI

Analyze and navigate through cryptocurrencies.

## Development setup

Install all dependencies:

    npm install

Run the development server:

    npm start

### Production setup

Since this repository is not publicly available yet, the Docker image needs to be build locally. So you might transfer the source code to the target machine and build it there.

Build the Docker image:

    docker build -t graphsense-gui .

Run it by passing it the URL of the [REST](https://github.com/graphsense/graphsense-REST) endpoint, e.g.: 

    docker run -e REST_ENDPOINT="https://example.com:9000" -p 8000:80 graphsense-gui
