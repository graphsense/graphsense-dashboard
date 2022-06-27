import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  server: { host: '0.0.0.0', hmr : { overlay : false } }
});
