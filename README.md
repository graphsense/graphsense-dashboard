# GraphSense GUI

Analyze and navigate through cryptocurrencies.

## Installation

Install all dependencies

    npm install

### Production setup

* Set `IS_DEV` in `webpack.config.js` to `false`. 
* Adapt `baseUrl` in `src/index.js`

Then run

    npm run build

Deploy `dist`.

### Development setup

Clone [graphsense-REST-python](https://git-service.ait.ac.at/dil-graphsense/graphsense-REST-Python) and switch to branch `cors`.

Having `docker` installed, run 

    docker/build.sh
    docker/start.sh

In `webpack.config.js` set `IS_DEV` to `true`. Then run

    npm start

to start the development server. Browse to `localhost:8080`.
