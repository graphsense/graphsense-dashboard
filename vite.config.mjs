import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import fs from 'fs';

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
  plugins: [elmPlugin({debug: true}), base64Loader],
  server: { 
    host: '0.0.0.0',
    port: 3000,
    hmr : { overlay : false }
  },
  publicDir: "generated/public",
  build: { 
    outDir: 'dist', 
    minify: 'terser',
    sourcemap: false
  },

});
