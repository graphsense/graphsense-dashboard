import { defineConfig, devices } from '@playwright/test'

// The port the built bundle is told to call. Nothing listens on it: every
// request is answered by `page.route` in e2e/fixtures.ts, so a request the
// fixtures do not cover fails loudly instead of reaching a real backend.
export const REST_URL = 'http://127.0.0.1:19999/api'

const PORT = 4173

export default defineConfig({
  testDir: './e2e',

  // These drive a real browser over a real build; they are slower and more
  // fragile than the elm-test suites by nature. Anything assertable in
  // `tests/Scenario` belongs there instead — see .claude/CLAUDE.md.
  timeout: 30_000,
  expect: { timeout: 10_000 },

  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  workers: process.env.CI ? 2 : undefined,
  reporter: process.env.CI ? [['list'], ['html', { open: 'never' }]] : [['list']],

  use: {
    baseURL: `http://127.0.0.1:${PORT}`,
    // A failing run should be diagnosable without reproducing it locally.
    trace: 'retain-on-failure',
    video: 'retain-on-failure',
    screenshot: 'only-on-failure'
  },

  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],

  // Serve the production build, not the dev server: the point of this layer is
  // to exercise what actually ships, including the vite plugins that rewrite
  // the compiled Elm kernel (see the elm-safe-virtual-dom notes in CLAUDE.md).
  webServer: {
    command: `npx vite preview --port ${PORT} --strictPort`,
    port: PORT,
    reuseExistingServer: !process.env.CI,
    timeout: 120_000
  }
})
