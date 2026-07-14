import { defineConfig, loadEnv } from "vite";
import elmPlugin from "vite-plugin-elm";
import fs from 'fs';
import { createFilter } from 'vite'

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
function elmSafeVirtualDomCheckPlugin() {
  const marker = '_VirtualDom_createTNode'
  const filter = createFilter(/\.elm$/)

  return {
    name: 'vite-plugin-elm-safe-virtual-dom-check',
    transform(code, id) {
      if (!filter(id)) return
      if (!code.includes(marker)) {
        this.warn('elm-safe-virtual-dom is NOT in this build — run `make virtual-dom-fix` (it may be cloned into the package directory of an older elm version, or elm-stuff may be stale)')
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
    minify: 'terser',
    sourcemap: false
  },

}));
