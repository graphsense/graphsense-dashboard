# GraphSense Dashboard

GraphSense's Web GUI for interactive cryptocurrency analysis, written in [Elm][elm] and
built with [Vite][vite].

The UI is assembled from components generated out of a Figma design, extended by optional
plugins that live in their own repositories. A fair amount of the tree is generated —
knowing which parts, and which `make` target regenerates them, is most of what you need to
work on it comfortably.

## Requirements

* [NodeJS][nodejs] 20 or newer (CI builds on 20.x)
* Python 3 — used by the code generators and the OpenAPI tooling
* `make`
* [pre-commit][pre-commit]
* Docker — only for the Docker build and the API proxy below

Elm itself is not a separate install; it comes in via npm and is run through `npx`.

## First-time setup

```bash
npm install
make install                              # installs the pre-commit and pre-push hooks
cp config/Config.elm.tmp config/Config.elm # plugin registration and theme
cp env.template .env                       # then set VITE_GS_REST_URL
```

`.env` needs at least `VITE_GS_REST_URL`, pointing at a [graphsense-lib Web API][graphsense-rest]
instance. Without it the app builds but every request fails. The full list is under
[Environment variables](#environment-variables). See
[Using the Iknaio backend via a proxy](#using-the-iknaio-backend-via-a-proxy) if you want
to develop against the upstream API.

You do **not** need Figma credentials to build: the design snapshot
(`theme/figma.json`) is committed. Only `make theme-refresh` talks to Figma.

## Running it

```bash
make serve      # dev server on http://localhost:3000
make build      # production build to ./dist
make compile    # type-check only
```

**Always go through `make`, never `npx vite` or `npm run dev` directly.** The make targets
export `ELM_HOME` so the compiler picks up the patched Elm packages, and they run the code
generators first. Bypassing them silently compiles against unpatched packages — see
[Patched Elm packages](#patched-elm-packages).

## Everyday commands

| Command | What it does |
|---|---|
| `make compile-quiet` | Type-check, printing only errors |
| `make format` | Format with elm-format (`src` and `tests`) |
| `make lint` | elm-review, including dead-code rules |
| `make lint-fix` | Apply elm-review's automatic fixes |
| `make test` | Elm test suite plus translation checks |
| `make e2e` | Browser tests (see [Testing](#testing)) |
| `make openapi` | Regenerate the REST client from the OpenAPI spec |
| `make api-fixtures` | Regenerate test fixtures from the spec examples |
| `make theme-refresh` | Re-fetch the Figma design and regenerate the theme |
| `make plugin-api` | Update the record of core symbols plugins use |
| `make clean-generated-plugins && make` | Rebuild plugin glue after adding or removing a plugin |
| `make print-plugins` | List the plugins currently registered |

`make install` wires `format`, `lint`, `test` and the plugin-API check into pre-commit, so
most of these run on commit anyway.

## Testing

Two layers. Run both before pushing; pre-commit runs the first.

```bash
make test           # Elm tests — fast, no browser, no network
make e2e-install    # once, to fetch the browser
make e2e            # browser tests (Playwright) — builds first
make e2e-ui         # the same, in Playwright's interactive mode
```

**`make test`** runs the Elm suite in about a second. `Update.update` and `View.view` are
pure and effects are plain data, so almost everything — user flows through the Pathfinder,
routing, decoders, formatting — is testable without a browser. Prefer this layer: anything
expressible here belongs here.

**`make e2e`** covers only what the Elm layer cannot reach: `src/main.js`, the ports, real
downloads and file pickers, keyboard chords the browser competes for, and whether the
bundle boots at all. Today that means

- the shipped bundle starts with no console errors and no uncaught exceptions,
- the elm-safe-virtual-dom patches are genuinely present in the build,
- saving a `.gs` file and reading it back, including via Ctrl/Cmd+S,
- settings surviving a reload, which proves the localStorage port ran.

It needs no API key and never reaches a real backend: the build points
`VITE_GS_REST_URL` at a port nothing listens on, and `e2e/fixtures.ts` answers every
outgoing request, matching on path rather than origin so the suite does not depend on your
`.env`. Fixture bodies must be complete enough to decode — the Elm client rejects a whole
response over one missing required field and logs it, and the boot test asserts the
console is clean, so a lazy stub fails loudly instead of passing quietly.

Selectors come from `Util.View.testId`, applied at component call sites and prefixed
`gs-`. elm-css class names are content hashes and visible text is translated and
truncated, so neither works as a selector.

## Generated code — never edit by hand

| Path | Generated from | Regenerate with |
|---|---|---|
| `elm.json` | `elm.json.base` + registered plugins | any `make` target |
| `generated/plugins/` | `plugin_templates/*.mustache` + `config/Config.elm` | `make clean-generated-plugins && make` |
| `generated/theme/` | `theme/figma.json` | `make theme-refresh` |
| `generated/utils/RecordSetter.elm` | all model types | `make setem` |
| `openapi/src/Api.elm` | the OpenAPI spec | `make openapi` |
| `tests/Fixtures/Api.elm` | spec response examples | `make api-fixtures` |
| `src/PluginApi.elm` | plugin sources | `make plugin-api` |
| `src/Version.elm` | the nearest git tag | pre-push hook |

Edits to these are lost on the next run. In particular, **prefer the generated Figma
components over hand-written CSS**, and take colours from `generated/theme/Theme/Colors.elm`
rather than introducing literals.

Re-run `make api-fixtures` after every `make openapi`, or the fixtures drift from the
client.

## Patched Elm packages

The build uses patched forks of `elm/virtual-dom`, `elm/browser`, `elm/html` and
`rtfeldman/elm-css` ([elm-safe-virtual-dom][safe-vdom]), cloned into `elm_packages/` by
`make virtual-dom-fix`. They stop the app from crashing when a browser extension — or
anything else outside Elm — modifies the DOM. **They are required; a build without them
has runtime DOM crashes.**

Two guards fail the build rather than let that ship: one checks the package sources the
compiler reads, the other checks the compiled output. If you see

```
elm-safe-virtual-dom is NOT in this build.
```

run `rm -rf elm-stuff && make build`. `elm-stuff` caches compiled dependencies by package
*version*, not content, so swapping a package's source underneath it does not invalidate
the cache. Changing the active plugin set rewrites `elm.json`, which is the usual trigger.

## Translations

Translations are YAML files in `lang/`, plus per-plugin files in `plugins/*/lang/`.
`make check-lang` (part of `make test`) fails when a locale has lost a key that
`lang/en.yaml` or a `View.Locale` call still uses. Note that `en.yaml` is an *override*
map, not a full key list — a missing lookup falls back to the key itself, which is the
English text. Known gaps are baselined in `lang/untranslated-baseline.json`; refresh with
`node tools/check_lang.mjs --update-baseline`.

## Develop plugins

Plugins are separate Git repositories, usually symlinked into `plugins/`. Each can have its
own `elm.json`, `package.json`, `lang/` and `public/`.

### Bootstrapping

Use `plugin_stub` as a starting point: `cp -r plugin_stub plugins/myplugin`

The plugin name is case insensitive. Its Elm package name is the plugin name with the first
letter capitalised, e.g. `Myplugin`.

Adapt the stub accordingly:
* rename `./Stub`, and `stub.js` to your plugin's name, keeping the respective capitalisation
* replace `Stub` in the stub Elm files with your plugin's Elm package

In order for the core system to call functions from your plugin, reference them in the respective modules in `./Stub/Interface`. See `./src/PluginInterface` for available hooks.

### Registering a plugin

1. Place or symlink the plugin in `plugins/`.
2. Run `make clean-generated-plugins && make`.

Plugins hook into the dashboard through `src/PluginInterface/` (View, Update, Effects,
Routes); the hook implementations go in the plugin's root module. See the comments in those
files for the details.

### Plugins and dead-code detection

`make lint` reports unused exports in the core. That check has a blind spot: plugins are
not always checked out, and `elm.json` is generated from whichever plugins are registered
in `config/Config.elm`. A core function that *only* a plugin uses therefore looks unused
whenever that plugin is absent — and CI has no plugins at all. Delete it on that evidence
and the plugin stops compiling.

`src/PluginApi.elm` resolves this. It is generated, contains no logic, and only
*references* every core symbol the plugins use, which is enough to keep those symbols
counted as used no matter what is checked out. As a side effect it is the written record of
what plugins depend on.

| | |
|---|---|
| `make plugin-api` | Adds newly-used core symbols to `src/PluginApi.elm`. Only ever adds, so it is safe to run with plugins missing. |
| `make check-plugin-api` | Fails if a plugin uses a core symbol not yet listed. Runs as a pre-commit hook. |
| `make plugin-api-prune` | Removes entries no longer used. Refuses unless every registered plugin is present. |
| `make lint` | Prints a note when plugins are not linked, warning that unused-export findings may be plugin-facing. |

Two rules of thumb:

- **Keep the symlink in `plugins/` even when you comment a plugin out of
  `config/Config.elm`.** The build follows the registration, the dead-code tooling follows
  the directory — so you get a fast build *and* accurate analysis.
- **If a plugin starts calling a new core function, run `make plugin-api` and commit the
  result.** Otherwise the next cleanup pass has no way to know that function is needed.

This protects a plugin-used symbol from being *deleted*, not from its signature changing. A
changed signature still breaks plugins at their next build.

#### The long-term fix, if this becomes painful

`PluginApi.elm` only *declares* the surface; plugins still import around 59 core modules
directly, so nothing stops the coupling growing. Turning it into a real **facade** would
close that: generate one `PluginApi.<Module>` per core module, mirroring the paths, and have
plugins import those instead — a one-line import change per plugin file.

The prize is not the boundary but the type annotations. Written out, the facade becomes a
compile-checked contract, so a changed core signature fails the build *in core, at commit
time* rather than surfacing in a plugin weeks later. Without annotations you get the
boundary and not the drift detection, which is the smaller half.

Two things to know before starting:

- **Elm cannot re-export another module's type constructors.** About 22 of them, across
  six modules, would need smart constructors in core (`Model.Dialog.centered` instead of
  exposing `Centered`) or a permanent allowlist.
- **Nothing enforces it.** Elm has no visibility rules inside an application, so a lint
  check would have to live in the plugin repositories — the only place that sees a new
  import at the moment it is written.

Not done, and not obviously worth it at the current rate of breakage (roughly one
`adapt to new <X> interface` commit every two months). Recorded so the option is not
rediscovered from scratch.

## Releases and versioning

Versions are calendar-based (`v26.08.0`, with `-dev.N` tags leading up to a release) and
live in git tags. The pre-push hook stamps the nearest tag into `src/Version.elm`, which is
what the status bar shows — so **tag before pushing**, or the hook rewrites the file and
asks you to commit it. `make tag-version VERSION=v26.08.1` writes the file, commits and
tags in one step.

Notable changes go in `CHANGELOG.md` under the current unreleased heading.

## Docker build

### Prerequisites

- [Docker][docker], see https://docs.docker.com/engine/install/
- Docker Compose: https://docs.docker.com/compose/install/

### Configuration

Copy the template and fill it in:

```bash
cp env.template .env
```

`docker-compose.yml` reads `DASHBOARD_PORT` (the host port; the container listens on 8000)
and `VITE_GS_REST_URL`, plus optional `DOCKER_IMAGE_NAME`, `DOCKER_CONTAINER_NAME` and
`DOCKER_HOSTNAME`.

### Usage

```bash
docker-compose build
docker-compose up -d
```

The dashboard is then at `http://localhost:$DASHBOARD_PORT` — port 8080 with the template
as shipped.

## Using the Iknaio backend via a proxy

For local development against the upstream Iknaio API, `tools/proxy-iknaio-api.sh` starts
an Nginx container on `http://localhost:8080` that injects your API key as an
`Authorization` header.

1. Export your API key:

       export GS_API_KEY="<your_iknaio_api_key>"

2. Start the proxy:

       ./tools/proxy-iknaio-api.sh

3. Point the dashboard at it, in `.env`:

       VITE_GS_REST_URL="http://localhost:8080"

   Optionally set the initial user-info endpoint path (empty disables the startup call):

       VITE_GS_USER_ENDPOINT_URL="/user"

4. Run the dashboard as usual (`make serve`).

Notes:

* The container is named `nginx-proxy-iknaio-prod-api`; remove it with
  `docker rm -f nginx-proxy-iknaio-prod-api`.
* CORS in the script is configured for `http://localhost:3000`, which is what `make serve`
  uses. Change the origin in the script if you serve from elsewhere.
* The proxy's port 8080 collides with `DASHBOARD_PORT` from the template. Change one of
  them if you run both at once.

## Environment variables

All of these live in `.env` (start from `env.template`). The Makefile does
`-include .env`, so the non-`VITE_` ones become make variables — which is why build
tooling settings sit in the same file as runtime ones.

### Runtime — compiled into the bundle

| Variable | Purpose | Default |
|---|---|---|
| `VITE_GS_REST_URL` | The [Web API][graphsense-rest] the app talks to. **Required** — without it the app builds but every request fails | none |
| `VITE_GS_USER_ENDPOINT_URL` | Path for the initial user-info request, e.g. `/user`. Leave empty to skip that call | none (skipped) |
| `VITE_LOGOUT_URL` | Where the logout button sends the user; substituted into `config/Config.elm` | none |

These are substituted at **build** time, not read at run time — changing one means
rebuilding. That is also why passing them to `docker run` has no effect.

### Build tooling — only needed for specific targets

| Variable | Needed for | Default |
|---|---|---|
| `OPENAPI_LOCATION` | `make openapi`, `make api-fixtures` — the spec to generate from, either a URL or a local path | `https://api.iknaio.com/openapi.json` |
| `REST_URL` | `make openapi` only | `https://app.iknaio.com` |
| `FIGMA_FILE_ID` | `make theme-refresh` | none |
| `FIGMA_API_TOKEN` | `make theme-refresh` | none |

Both defaults apply whether the variable is unset *or* set to an empty value, which
matters because `env.template` ships `OPENAPI_LOCATION=` blank.

`REST_URL` is not a backend setting despite the name: `make openapi` writes it into the
spec's `servers` list so the generator emits it as the client's base path, then replaces
it with the `{{VITE_GS_REST_URL}}` placeholder. The value is arbitrary and you can leave
it unset.

Neither Figma variable is needed for a normal build — `theme/figma.json` is committed, and
only `make theme-refresh` contacts Figma.

### Docker Compose

| Variable | Purpose | Default |
|---|---|---|
| `DASHBOARD_PORT` | Host port to publish; the container listens on 8000 | none — `docker-compose up` fails without it |
| `DOCKER_IMAGE_NAME` | Image name | `graphsense-dashboard` |
| `DOCKER_CONTAINER_NAME` | Container name | `graphsense-dashboard` |
| `DOCKER_HOSTNAME` | Container hostname | `graphsense-dashboard` |

Plugins bring their own `VITE_*` variables for their backends; those are documented by the
plugin that reads them.

## Troubleshooting

**`elm-safe-virtual-dom is NOT in this build`** — `rm -rf elm-stuff && make build`. See
[Patched Elm packages](#patched-elm-packages).

**`MODULE NOT FOUND` for a plugin module** — the plugin is registered in
`config/Config.elm` but missing from `plugins/`. Either link it, or comment out its
registration.

**Changed `config/Config.elm` and the build still disagrees** — `elm.json` and the plugin
glue are generated, and a bare `elm make` or `npx vite` uses whatever is on disk. Run a
`make` target so they regenerate.

**`TypeError: Node.removeChild: Argument 1 is not an object`** — the app is running without
the virtual-dom patches; same fix as above.

**Elm compiler upgrade broke the patched packages** — they live in
`elm_packages/<elm version>/`, so a new compiler version looks in a fresh, unpatched
directory. `make virtual-dom-fix` re-clones them.

---

For architecture, testing conventions and the reasoning behind the generated pipeline, see
`.claude/CLAUDE.md`.

[elm]: https://elm-lang.org
[vite]: https://vite.dev
[nodejs]: https://nodejs.org
[pre-commit]: https://pre-commit.com/#install
[graphsense-rest]: https://github.com/graphsense/graphsense-lib
[docker]: https://www.docker.com
[safe-vdom]: https://github.com/lydell/virtual-dom/tree/safe
