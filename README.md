# GraphSense Dashboard

A web dashboard for interactive cryptocurrency analysis.

## Development setup

You need to have [NodeJS][nodejs] installed. It comes with [NPM][npm],
the package manager for JavaScript.

In order to install all dependencies, run the following from the root of
this repository:

    npm install

Adapt `DEV_REST_ENDPOINT` in `webpack.config.js` to point to your
development [graphsense-REST][graphsense-rest] service.

Additionally you may add the Titanium Report Generation Webservice through
`DEV_TITANIUM_REPORT_GENERATION_URL`.

Then start the development server:

    npm start

Point your browser to `localhost:8080`.

## Production setup

### Prerequisites

Install Docker and Docker Compose:

- [Docker][docker], see e.g. https://docs.docker.com/engine/install/
- Docker Compose: https://docs.docker.com/compose/install/

This service assumes that:
 - There is a cassandra instance running;
 - Both parser and exporter from `graphsense-blocksci` have completed fetching data into that cassandra instance.
 - The transformation pipeline has completed.
 - The Graphsense REST service is running, connected to the same cassandra database.
 
**It is possible to set up all required services using a single docker-compose evironment. For that, check out the `graphsense-setup` project.** Alternatively, you can set up each required service manually, in which case, keep on reading.

### Configuration

Copy `env.example` to `.env`:

    cp env.example .env

Edit the file `.env` and set the URL of the [graphsense-REST][graphsense-rest]
service, e.g.:

    REST_ENDPOINT="https://example.com:9000"

Additional environment variables:

* `TITANIUM_REPORT_GENERATION_URL`: The webservice URL for generating
  Titanium JSON/PDF Reports (optional).

Make sure to apply the configuation by adding this line to `docker-compose.yml`:
```yaml
services:
    graphsense-dashboard:
        ...
        env_file: .env
        ...
```

### Usage

Build the Docker image:

    docker-compose build

Start a container (in detached mode):

    docker-compose up -d

Finally, test the application in a web browser:

    http://localhost:8000

The default port (8000) can be changed in the `.env` file.

## Color configuration

You can map tag concepts to colors in `./config/conceptsColors.yaml`.
The file itself contains hints on the format.

This file is deployed as is. You can easily replace it at runtime in the
deployed directory.

[nodejs]: https://nodejs.org
[npm]: https://www.npmjs.com
[graphsense-rest]: https://github.com/graphsense/graphsense-rest
[docker]: https://www.docker.com
