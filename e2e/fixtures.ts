import { test as base, expect, type Page, type Route } from '@playwright/test'

/**
 * Every request that leaves the preview origin is answered here. Nothing
 * reaches a real backend, whatever `VITE_GS_REST_URL` happens to be in the
 * developer's `.env` — the routing matches on path, not on origin, precisely so
 * the suite does not depend on that.
 *
 * The bodies are shaped like the ones the OpenAPI spec documents. They have to
 * be: the Elm client rejects a whole response over one missing required field
 * and logs the failure to the console, which `boot.spec.ts` asserts is empty.
 * A lazy stub here surfaces as a failing boot test rather than a passing lie.
 */
export const ADDRESS = '1Archive1n2C579dMsAu3iC6tWzuQJz8dN'

const values = (v: number) => ({ value: v, fiat_values: [{ code: 'usd', value: v / 100 }] })

const txSummary = {
  height: 47,
  timestamp: 1625703347,
  tx_hash: '04d92601677d62a985310b61a301e74870fa942c8be0648e16b1db23b996a8cd'
}

const NETWORKS = ['btc', 'bch', 'ltc', 'zec', 'eth', 'trx']

/** path -> response body. Paths are matched exactly, after the origin. */
const RESPONSES: Record<string, unknown> = {
  '/stats': {
    currencies: NETWORKS.map((name) => ({
      name,
      no_blocks: 750000,
      no_address_relations: 1000000,
      no_addresses: 500000,
      no_entities: 200000,
      no_txs: 800000,
      no_labels: 10000,
      no_tagged_addresses: 5000,
      timestamp: 1625703347,
      schema_type: name === 'eth' || name === 'trx' ? 'account' : 'utxo'
    })),
    version: '1.0.0',
    request_timestamp: '2026-07-28T12:00:00'
  },
  '/tags/taxonomies/entity/concepts': [],
  '/tags/taxonomies/abuse/concepts': [],
  '/btc/clusters/264711': {
    currency: 'btc',
    cluster: 264711,
    entity: 264711,
    root_address: ADDRESS,
    balance: values(1000000),
    total_received: values(2000000),
    total_spent: values(1000000),
    first_tx: txSummary,
    last_tx: txSummary,
    in_degree: 100,
    out_degree: 50,
    no_addresses: 1,
    no_address_tags: 0,
    no_incoming_txs: 200,
    no_outgoing_txs: 100
  },
  [`/btc/addresses/${ADDRESS}`]: {
    address: ADDRESS,
    currency: 'btc',
    cluster: 264711,
    entity: 264711,
    balance: values(1000000),
    total_received: values(2000000),
    total_spent: values(1000000),
    first_tx: txSummary,
    last_tx: txSummary,
    in_degree: 100,
    out_degree: 50,
    no_incoming_txs: 200,
    no_outgoing_txs: 100,
    status: 'clean'
  }
}

for (const network of NETWORKS) {
  RESPONSES[`/${network}/supported_tokens`] = { token_configs: [] }
}

/**
 * Empty-but-well-shaped answers for the collection endpoints, matched by suffix.
 * A single address view pulls half a dozen of these; listing every one by full
 * path would make this file a transcript of the app's request log.
 */
const DEFAULTS: Array<[RegExp, unknown]> = [
  [/\/tag_summary$/, { broad_category: '', tag_count: 0, concept_tag_cloud: {}, label_summary: {} }],
  [/\/tags$/, { address_tags: [] }],
  [/\/related_addresses$/, { related_addresses: [] }],
  [/\/neighbors$/, { neighbors: [] }],
  [/\/txs$/, { address_txs: [], next_page: null }],
  [/\/links$/, { links: [] }],
  // the bulk endpoints opening a .gs file uses to refill nodes; they answer
  // with a list of rows rather than an object
  [/\/bulk/, []]
]

/** Chromium logs these itself for any non-2xx; they say nothing about the app. */
const NETWORK_NOISE = /^Failed to load resource/

type Fixtures = {
  /** Uncaught exceptions and app-level console errors since the page opened. */
  problems: string[]
  /** Cross-origin paths no fixture describes. */
  unmocked: string[]
  /** A page with the network sealed off and errors recorded. */
  app: Page
}

export const test = base.extend<Fixtures>({
  problems: async ({}, use) => await use([]),
  unmocked: async ({}, use) => await use([]),

  app: async ({ page, baseURL, problems, unmocked }, use) => {
    page.on('console', (msg) => {
      if (msg.type() === 'error' && !NETWORK_NOISE.test(msg.text())) {
        problems.push(`console.error: ${msg.text()}`)
      }
    })
    page.on('pageerror', (error) => problems.push(`pageerror: ${error.message}`))

    await page.route('**/*', async (route: Route) => {
      const url = new URL(route.request().url())

      // Matched on path, before anything else: depending on VITE_GS_REST_URL
      // the API calls are cross-origin or relative to the preview server, and
      // a relative one would otherwise be answered by vite's SPA fallback with
      // index.html, which the client then tries to parse as JSON.
      const path = url.pathname.replace(/^\/api/, '')

      const body = RESPONSES[path]
      if (body !== undefined) return route.fulfill({ json: body })

      const fallback = DEFAULTS.find(([pattern]) => pattern.test(path))
      if (fallback) return route.fulfill({ json: fallback[1] })

      // The app itself, served by vite preview.
      if (baseURL && url.origin === new URL(baseURL).origin) return route.continue()

      // Answer rather than abort: an aborted request makes Chromium log a
      // resource error, which would drown the console assertions. Record it so
      // a test can be explicit about coverage if it wants to be.
      unmocked.push(url.pathname)
      return route.fulfill({ json: {} })
    })

    await use(page)
  }
})

export { expect }

/**
 * One specific address node.
 *
 * `getByTestId('gs-address-node')` matches every node on the graph, and
 * Playwright's strict mode fails any single-element assertion on a locator that
 * matches more than one — so reach for this whenever a test means a particular
 * address. Use the bare test id for counting.
 */
export function addressNode (page: Page, address: string, network = 'btc') {
  return page.locator(
    `[data-testid="gs-address-node"][data-testkey="${network}${address}"]`
  )
}

/** Resolves once Elm has rendered the graph. */
export async function waitForPathfinder (page: Page): Promise<void> {
  await page.waitForSelector('#graph', { state: 'attached' })
}
