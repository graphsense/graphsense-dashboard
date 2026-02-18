# GraphSense Dashboard

GraphSense's Web GUI for interactive cryptocurrency analysis written in [Elm](https://elm-lang.org).

## Requirements

* [NodeJS][nodejs]
* [pre-commit][pre-commit]

## Configuration

Run `cp config/Config.elm.tmp config/Config.elm` and optionally configure plugins and your custom theme here.

## Install plugins

1. Place plugins in the `plugins` folder.
2. Run `make clean-generated-plugins && make`
3. Configure the plugin in `config/Config.elm`, eg:

```elm
import Myplugin

...

plugins : Plugin.Plugins
plugins =
    Plugin.empty
    |> Plugin.myplugin (Myplugin.plugin {- plugin specific arguments here -})
```

## Development setup

Run `make serve`. It starts Vite's development server.

## Production build

Run `make build`. It builds the app together with all configured plugins to `./dist`.

## Develop plugins

### Bootstrapping

Use `plugin_stub` as a starting point: `cp -r plugin_stub plugins/myplugin`

The name of the plugin is case insensitive. Elm package name of the plugin is the plugin name with the first letter capitalized, eg. `Myplugin`.

Adapt the stub accordingly:
* rename `./Stub`, `./Stub.elm` and `stub.js` to your plugin's name. Keep the respective capitalization. 
* replace `Stub` in the stub Elm files with your plugin's Elm package.

Place plugin specific dependencies in plugin's `./dependencies.txt`.

### Development

Plugins can hook into the dashboard functionality in order to extend it.

Plugin's hook implementations need to be set in your plugin's root module which was derived from `./Stub.elm`.
Please see the comments in the respective files of `./src/PluginInterface` for detailed documentation.

## Docker build

### Prerequisites

Install Docker and Docker Compose:

- [Docker][docker], see e.g. https://docs.docker.com/engine/install/
- Docker Compose: https://docs.docker.com/compose/install/

### Configuration

Copy `docker/env.template` to `.env`:

    cp docker/env.template .env

Edit the file `.env` and set the URL of the [graphsense-lib Web Api][graphsense-rest]
service, e.g.:

    VITE_GS_REST_URL="https://api.iknaio.com"

### Using Iknaio Backend via a Proxy

For local development against the upstream Iknaio API, use
`tools/proxy-iknaio-api.sh`. The script starts an Nginx container on
`http://localhost:8080` and injects your API key as `Authorization` header.

1. Export your Iknaio API key:

    export GS_API_KEY="<your_iknaio_api_key>"

2. Start the proxy:

    ./tools/proxy-iknaio-api.sh

3. Point the dashboard to the local proxy (for example in `.env`):

    VITE_GS_REST_URL="http://localhost:8080"

4. Run the dashboard as usual (`make serve` or Docker setup).

Notes:

* The proxy container name is `nginx-proxy-iknaio-prod-api`.
* CORS in the script is currently configured for `http://localhost:3000`.
  If you use a different frontend dev origin (for example Vite default
  `http://localhost:5173`), update the origin in
  `tools/proxy-iknaio-api.sh`.
* Stop/remove the proxy container with:

      docker rm -f nginx-proxy-iknaio-prod-api


### Usage

Build the Docker image:

    docker-compose build

Start a container (in detached mode):

    docker-compose up -d

Finally, test the application in a web browser:

    http://localhost:8000

[nodejs]: https://nodejs.org
[pre-commit]: https://pre-commit.com/#install
[npm]: https://www.npmjs.com
[graphsense-rest]: https://github.com/graphsense/graphsense-lib
[docker]: https://www.docker.com
