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

### Configuration

Copy `docker/env.template` to `.env`:

    cp docker/env.template .env

Edit the file `.env` and set the URL of the [graphsense-REST][graphsense-rest]
service, e.g.:

    REST_ENDPOINT="https://example.com:9000"

Additional environment variables:

* `TITANIUM_REPORT_GENERATION_URL`: The webservice URL for generating
  Titanium JSON/PDF Reports (optional).

### Usage

Build the Docker image:

    docker-compose build

Start a container (in detached mode):

    docker-compose up -d

Finally, test the application in a web browser:

    http://localhost:8000

## Generate static site

Static pages are not generated in development mode. The reason is that
Webpack's development server does not work well with the static-site-generator
plugin.

First, make sure `DEV_REST_ENDPOINT` in `webpack.config.js` points to a
[graphsense-REST][graphsense-rest] service. This is needed to fetch
cryptocurrency statistics at build time.

Then, to generate static pages, run the following command:

    npm run official

Then deploy the directory `official`. It contains everything for the
static website.

Static pages can be edited in `src/pages/static`.

## Color configuration

You can map tag categories to colors in `./config/categoryColors.yaml`.
The file itself contains hints on the format.

This file is deployed as is. You can easily replace it at runtime in the
deployed directory.

[nodejs]: https://nodejs.org
[npm]: https://www.npmjs.com
[graphsense-rest]: https://github.com/graphsense/graphsense-rest
