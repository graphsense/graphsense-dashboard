import { defineConfig, loadEnv } from "vite";
import elmPlugin from "vite-plugin-elm";
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { createFilter } from 'vite'

// The elm compiler that vite-plugin-elm spawns reads its packages from
// ELM_HOME. The Makefile exports it, so `make serve` gets the patched
// elm-safe-virtual-dom clones — but `npm run dev`/`npx vite` does not, and elm
// then silently resolves the unpatched registry packages from ~/.elm. Pin it
// here so the entry point stops mattering.
process.env.ELM_HOME ??= path.resolve(path.dirname(fileURLToPath(import.meta.url)), 'elm_packages')

function envReplacePlugin(options = {}) {
  const filter = createFilter(options.include || /\.(js|ts)$/, options.exclude)

  return {
    name: 'vite-plugin-env-replace',
    transform(code, id) {
      if (!filter(id)) return

      return {
        code: replacePlaceholders(code),
        map: null
      }
    }
  }
}

function replacePlaceholders(code) {
  const envVars = loadEnv(process.env.NODE_ENV, process.cwd())
  const placeholderRegex = /\{\{([A-Za-z_][A-Za-z0-9_]*)\}\}/g

  return code.replace(placeholderRegex, (match, placeholder) => {
    return envVars[placeholder] !== undefined
      ? envVars[placeholder]
      : match
  })
}

// Elm's `_Platform_initialize` renders the first frame synchronously inside
// `var stepper = stepperBuilder(sendToApp, model);`. If that first render
// synchronously dispatches a DOM event back into the app (e.g. an iframe
// firing `load` on append, or a custom element dispatching from its
// `connectedCallback`), `sendToApp` runs before `stepper` is assigned and
// crashes with "TypeError: stepper is not a function", aborting the render
// mid-patch. This surfaced on hard reloads of the case-connect UI.
// Rewriting the compiled bundle here (instead of patching elm/core in
// ELM_HOME) survives package re-downloads and needs no artifacts.dat
// cache-busting; the message is processed normally and the view step is
// deferred to a microtask, by which time `stepper` exists.
function elmStepperGuardPlugin() {
  const needle = 'stepper(model = pair.a, viewMetadata);'
  const guard =
    'model = pair.a;\n' +
    '\t\t// GS_STEPPER_GUARD (injected by vite, see vite.config.mjs)\n' +
    '\t\tif (stepper) { stepper(model, viewMetadata); }\n' +
    '\t\telse { Promise.resolve().then(function () { stepper(model, viewMetadata); }); }'
  const filter = createFilter(/\.elm$/)

  return {
    name: 'vite-plugin-elm-stepper-guard',
    transform(code, id) {
      if (!filter(id)) return
      if (!code.includes(needle)) {
        if (code.includes('_Platform_initialize')) {
          this.warn('elm-stepper-guard: kernel pattern not found — the stepper race is NOT patched (elm/core kernel changed?)')
        }
        return
      }
      return { code: code.replaceAll(needle, guard), map: null }
    }
  }
}

// The Makefile (`virtual-dom-fix`) clones elm-safe-virtual-dom over elm's
// virtual-dom/html/browser/elm-css, so the app survives DOM changes made by
// browser extensions. Nothing else notices when those clones do not reach the
// compiler — an elm upgrade moves the package cache to a new directory, a
// stale elm-stuff keeps the old artifacts — and the app is then one extension
// away from "Node.removeChild: Argument 1 is not an object". So: check.
// This is the last word on what actually ships: `make check-virtual-dom-fix`
// inspects the *clones*, so it keeps passing while the compiled output has no
// patch in it at all. That combination has shipped unpatched bundles more than
// once — the most reliable trigger is a stale elm-stuff after tools/generate.js
// rewrites elm.json, which happens whenever the active plugin set changes.
//
// It fails the build rather than warning: a warning scrolls past in vite's
// output, and the consequence of missing it is a runtime crash
// ("Node.removeChild: Argument 1 is not an object") the first time a browser
// extension or a keyed list touches the DOM.
function elmSafeVirtualDomCheckPlugin() {
  const marker = '_VirtualDom_createTNode'
  const filter = createFilter(/\.elm$/)

  return {
    name: 'vite-plugin-elm-safe-virtual-dom-check',
    transform(code, id) {
      if (!filter(id)) return
      if (!code.includes(marker)) {
        this.error(
          'elm-safe-virtual-dom is NOT in this build.\n\n' +
            '  Without it the app crashes whenever anything outside Elm touches the DOM.\n' +
            '  Usually a stale build cache — try:\n\n' +
            '    rm -rf elm-stuff && make build\n\n' +
            '  If that does not help, the patched packages may be missing or cloned into\n' +
            '  the package directory of a different elm version: run `make virtual-dom-fix`.'
        )
      }
    }
  }
}

/** @type {import('vite').Plugin} */
const base64Loader = {
  name: 'base64-loader',
  transform(code, id) {
      const [path, query] = id.split('?');
      if (query != 'raw-base64')
          return null;

      const data = fs.readFileSync(path);
      const hex = data.toString('base64');

      return `export default '${hex}';`;
  }
};

export default defineConfig(({ command }) => ({
  // The Elm time-travel debugger (dev only, never in builds) crashes on large
  // models: its Expando walks the whole model on every update and overflows
  // the stack in Firefox ("InternalError: too much recursion",
  // https://github.com/elm/virtual-dom/issues/80), freezing the app. If that
  // bites you, disable it with ELM_DEBUGGER=false.
  plugins: [elmPlugin({ debug: command === 'serve' && process.env.ELM_DEBUGGER !== 'false' }), elmStepperGuardPlugin(), elmSafeVirtualDomCheckPlugin(), base64Loader, envReplacePlugin({include: [/\.elm$/, /src\/main\.js$/], exclude: /node_modules/})],
  server: { 
    host: '0.0.0.0',
    port: 3000,
    hmr : { overlay : true }
  },
  worker: { format: 'es' },
  publicDir: "generated/public",
  build: {
    manifest: true,
    outDir: 'dist',
    // Rolldown's oxc minifier, which is vite 8's default. The explicit
    // 'terser' this replaces dates from the vite 5 era, when the choice was
    // terser or esbuild and terser won on size; the rolldown switch in vite 8
    // changed the default underneath it and the override was never revisited.
    // Measured on the all-plugins bundle: 4.1s vs terser's 16.5s, output 5.7%
    // smaller raw and within 1.6% gzipped. Note the saving is almost entirely
    // in the non-elm chunks (jspdf, html2canvas, the svg-to-pdf worker) — the
    // elm bundle itself came out 0.6% smaller, i.e. minified near-identically.
    minify: true,
    sourcemap: false
  },

}));
