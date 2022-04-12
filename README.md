# GraphSense Dashboard

A web dashboard for interactive cryptocurrency analysis.

## Development setup

You need to have [NodeJS][nodejs] installed. It comes with [NPM][npm],
the package manager for JavaScript.

In order to install all dependencies, run the following from the root of
this repository:

    npm install

Then start the [vite](https://vitejs.dev) development server:

    npm run dev

Point your browser to `localhost:3000`.

## Testing

Run
    
    make watch

to watch for changes in elm files and openapi templates. Also regenerates the openapi client (see `./openapi`).

* TODO: explain directories
* TODO: explain testing

## Production setup

### Prerequisites

Install Docker and Docker Compose:

- [Docker][docker], see e.g. https://docs.docker.com/engine/install/
- Docker Compose: https://docs.docker.com/compose/install/

### Configuration

Copy `docker/env.template` to `.env`:

    cp docker/env.template .env

Edit the file `.env` and set the URL of the [graphsense-REST][graphsense-rest]
service, e.g.:

    REST_ENDPOINT="https://example.com:9000"

### Usage

Build the Docker image:

    docker-compose build

Start a container (in detached mode):

    docker-compose up -d

Finally, test the application in a web browser:

    http://localhost:8000

## Color configuration

You can map tag concepts to colors in `./config/conceptsColors.yaml`.
The file itself contains hints on the format.

This file is deployed as is. You can easily replace it at runtime in the
deployed directory.

[nodejs]: https://nodejs.org
[npm]: https://www.npmjs.com
[graphsense-rest]: https://github.com/graphsense/graphsense-rest
[docker]: https://www.docker.com
