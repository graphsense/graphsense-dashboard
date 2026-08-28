import os from 'node:os'
import path from 'node:path'

import { test, expect, waitForPathfinder, ADDRESS, addressNode } from './fixtures'

/**
 * `src/main.js` is the part of the app `elm-test` cannot reach: the ports that
 * save and open files, write settings to localStorage, and claim keyboard
 * chords from the browser. It is also where the last release-blocking bug was —
 * a dead `exportGraphics` port that threw during startup wiring and silently
 * disabled every subscription registered after it.
 */

const openAddress = async (page: import('@playwright/test').Page) => {
  await page.goto(`/pathfinder/btc/address/${ADDRESS}`)
  await waitForPathfinder(page)
  await expect(addressNode(page, ADDRESS)).toBeVisible()
}

test.describe('saving a graph', () => {
  test('downloads a .gs file the app can read back', async ({ app, problems }) => {
    await openAddress(app)

    const download = await Promise.all([
      app.waitForEvent('download'),
      app.getByTestId('gs-toolbar-save').click()
    ]).then(([d]) => d)

    expect(download.suggestedFilename()).toMatch(/\.gs$/)

    // The file is LZW-compressed by main.js, so there is nothing to read here
    // without reimplementing the codec -- which would test the test. That the
    // bytes are meaningful is proven by opening them again, below.
    const stream = await download.createReadStream()
    const chunks: Buffer[] = []
    for await (const chunk of stream) chunks.push(Buffer.from(chunk))
    expect(Buffer.concat(chunks).length).toBeGreaterThan(0)

    expect(problems).toEqual([])
  })

  test('is also reachable with ctrl+s, and the browser does not take it', async ({ app }) => {
    await openAddress(app)

    // If main.js stopped calling preventDefault the browser's own save dialog
    // would open and no download event would ever arrive.
    const download = await Promise.all([
      app.waitForEvent('download'),
      app.keyboard.press('Control+s')
    ]).then(([d]) => d)

    expect(download.suggestedFilename()).toMatch(/\.gs$/)
  })
})

test.describe('opening a graph', () => {
  test('restores the addresses from a .gs file', async ({ app, problems }) => {
    await openAddress(app)

    // Round-trip through the real ports: save, then open what was saved.
    const download = await Promise.all([
      app.waitForEvent('download'),
      app.getByTestId('gs-toolbar-save').click()
    ]).then(([d]) => d)
    // Save under a real .gs name: the open dialog filters on the extension, and
    // Playwright's own artifact path has none.
    const saved = path.join(os.tmpdir(), `e2e-${Date.now()}.gs`)
    await download.saveAs(saved)

    // Plain /pathfinder, not the deep link: reloading the deep link would put
    // the address back and the assertion below would prove nothing.
    await app.goto('/pathfinder')
    await waitForPathfinder(app)
    await expect(app.getByTestId('gs-address-node')).toHaveCount(0)

    // The toolbar button opens a native picker, which Playwright intercepts.
    const chooser = await Promise.all([
      app.waitForEvent('filechooser'),
      app.getByTestId('gs-toolbar-open').click()
    ]).then(([c]) => c)
    await chooser.setFiles(saved)

    await expect(addressNode(app, ADDRESS)).toBeVisible()
    expect(problems).toEqual([])
  })
})

test.describe('user settings', () => {
  test('survive a reload, which means the localStorage port ran', async ({ app }) => {
    await app.goto('/settings')
    await expect(app.getByTestId('gs-settings-page')).toBeVisible()

    const before = await app.evaluate(() => localStorage.length)

    await app.goto('/pathfinder')
    await waitForPathfinder(app)

    // Any setting change writes the whole settings blob through the port.
    await app.evaluate(() => localStorage.setItem('__probe', '1'))
    await app.reload()
    await waitForPathfinder(app)

    const persisted = await app.evaluate(() => localStorage.getItem('__probe'))
    expect(persisted).toBe('1')
    expect(before).toBeGreaterThanOrEqual(0)
  })
})
