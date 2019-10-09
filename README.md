# GraphSense Dashboard

A Web dashboard for interactive cryptocurrency analysis.

## Development setup

You need to have [NodeJS][nodejs] installed. It comes with [NPM][npm],
the package manager for JavaScript.

In order to install all dependencies, run the following from the root of this repository:

    npm install

Adapt `DEV_REST_ENDPOINT` in `webpack.config.js` to point to your development
[graphsense-REST][graphsense-rest] service.

Then start the development server:

    npm start

Point your browser to `localhost:8080`.

### A note on static pages

Static pages are not generated in development mode. The reason is that
Webpack's development server does not work well with the static-site-generator
plugin.

To generate static pages, make a local production build (see below) and deploy the directory `official`. It contains everything for the static website.

Static pages can be edited in `src/pages/static`.

## Production setup

Build the Docker image:

    docker build -t graphsense-dashboard .

Run it by passing it the URL of the [graphsense-REST][graphsense-rest]
service, e.g.: 

    docker run -e REST_ENDPOINT="https://example.com:9000" -p 8000:80 graphsense-dashboard

## Local production setup

If you don't want to use docker or want to generate the static site, create a local production build: 

    npm run build

Build destination of the application is `dist`, for the static site it's `official`.

The local production build uses `DEV_REST_ENDPOINT`.

## Color configuration

You can map tag categories to colors in `./config/categoryColors.yaml`. The file contains hints on the format.

This file deployed as is. You can easily replace it at runtime in the deployed directory.

[nodejs]: https://nodejs.org
[npm]: https://www.npmjs.com
[graphsense-rest]: https://github.com/graphsense/graphsense-rest
