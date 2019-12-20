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

## Production setup

Build the Docker image:

    docker build -t graphsense-dashboard .

Run it by passing it the URL of the [graphsense-REST][graphsense-rest]
service, e.g.: 

    docker run -e REST_ENDPOINT="https://example.com:9000" -p 8000:8000 graphsense-dashboard

## Generate static site

Static pages are not generated in development mode. The reason is that Webpack's development server does not work well with the static-site-generator plugin.

First, make sure `DEV_REST_ENDPOINT` in `webpack.config.js` points to a [graphsense-REST][graphsense-rest] service. This is needed to fetch cryptocurrency statistics at build time.

Then, to generate static pages, run the following command:

    npm run official

Then deploy the directory `official`. It contains everything for the static website.

Static pages can be edited in `src/pages/static`.

## Color configuration

You can map tag categories to colors in `./config/categoryColors.yaml`. The file itself contains hints on the format.

This file is deployed as is. You can easily replace it at runtime in the deployed directory.

[nodejs]: https://nodejs.org
[npm]: https://www.npmjs.com
[graphsense-rest]: https://github.com/graphsense/graphsense-rest
