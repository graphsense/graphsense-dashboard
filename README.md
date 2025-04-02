# GraphSense Dashboard

GraphSense's Web GUI for interactive cryptocurrency analysis written in [Elm](https://elm-lang.org).

## Requirements

* [NodeJS][nodejs]
* [pre-commit][pre-commit]

## Configuration

Run `cp config/Config.elm.tmp config/Config.elm` and optionally configure plugins and your custom theme here.

## Install plugins

1. Place plugins in the `plugins` folder.
2. Run `make clean-plugins && make`
3. Configure the plugin in `config/Config.elm`, eg:

```elm

plugins : Plugin.Plugins
plugins =
    Plugin.empty
    |> Plugin.myplugin (Myplugin.plugin {- plugin specific arguments here -})
```

## Development setup

Run `make serve`. It starts Vite's development server.

## Production build

Run `make build`. It builds the app together with all plugins to `./dist`.

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

Edit the file `.env` and set the URL of the [graphsense-REST][graphsense-rest]
service, e.g.:

    REST_URL="https://api.ikna.io"

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
[graphsense-rest]: https://github.com/graphsense/graphsense-rest
[docker]: https://www.docker.com
