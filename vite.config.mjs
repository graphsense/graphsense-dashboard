import { defineConfig, loadEnv } from "vite";
import elmPlugin from "vite-plugin-elm";
import fs from 'fs';
//import plugins_vite from '../generated/plugins/vite.config.js'
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
  const placeholderRegex = /\{\{\s*([A-Z0-9_]+)\s*\}\}/g

  return code.replace(placeholderRegex, (match, placeholder) => {
    return envVars[placeholder] !== undefined
      ? envVars[placeholder]
      : match
  })
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

export default defineConfig({
  plugins: [elmPlugin(), base64Loader, envReplacePlugin({include: /\.elm$/})],
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

});
