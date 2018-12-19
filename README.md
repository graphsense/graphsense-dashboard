# GraphSense GUI

Analyze and navigate through cryptocurrencies.

## Development setup

You need to have [NodeJS](https://nodejs.org) installed. It comes with [NPM](https://www.npmjs.com), the package manager for JavaScript.

In order to install all dependencies run from the root of this repository:

    npm install

Adapt `DEV_REST_ENDPOINT` in `webpack.config.js` to point to your development [graphsense-REST](https://github.com/graphsense/graphsense-REST) service.

Then start the development server:

    npm start

Point your browser to `localhost:8080`.

### A note on static pages

Static pages are not generated in development mode. The reason is that Webpack's development server does not work well with the static-site-generator plugin.

Static pages are located in `src/pages/static`.

## Production setup

Build the Docker image:

    docker build -t graphsense-gui .

Run it by passing it the URL of the [graphsense-REST](https://github.com/graphsense/graphsense-REST) service, e.g.: 

    docker run -e REST_ENDPOINT="https://example.com:9000" -p 8000:80 graphsense-gui

## Build official website

The official website consists of static pages without the app. Edit its contents at `src/pages/static/officialpage.html`.

In order to build it run:

    npm run build

Then deploy the contents of `official/`. It contains the static content without any JS files.
