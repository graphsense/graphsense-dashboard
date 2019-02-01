# GraphSense Dashboard

A Web dashboard for interactive cryptocurrency analysis.

## Development setup

You need to have [NodeJS][nodejs] installed. It comes with [NPM][npm],
the package manager for JavaScript.

In order to install all dependencies run from the root of this repository:

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

Static pages are located in `src/pages/static`.

## Production setup

Build the Docker image:

    docker build -t graphsense-dashboard .

Run it by passing it the URL of the [graphsense-REST][graphsense-rest]
service, e.g.: 

    docker run -e REST_ENDPOINT="https://example.com:9000" -p 8000:80 graphsense-dashboard


[nodejs]: https://nodejs.org
[npm]: https://www.npmjs.com
[graphsense-rest]: https://github.com/graphsense/graphsense-rest
