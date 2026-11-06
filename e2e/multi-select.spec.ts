import { test, expect, waitForPathfinder, ADDRESS, addressNode } from './fixtures'

/**
 * What `elm-test` cannot reach: that the subscriptions are wired, and what a
 * real browser reports on its mouse events. The toolbar's highlight is the
 * visible state of the mode, and the only one a user has.
 */

const HIGHLIGHT = 'rgb(192, 222, 247)'

const openAddress = async (page: import('@playwright/test').Page) => {
  await page.goto(`/pathfinder/btc/address/${ADDRESS}`)
  await waitForPathfinder(page)
  await expect(addressNode(page, ADDRESS)).toBeVisible()
}

test.describe('multi-select with Ctrl', () => {
  test('shows on the toolbar while the key is held', async ({ app }) => {
    await openAddress(app)
    const tool = app.getByTestId('gs-toolbar-select')

    await app.keyboard.down('Control')
    await expect(tool).toHaveCSS('background-color', HIGHLIGHT)

    // the key really is down, so moving the mouse must not end the mode
    await app.mouse.move(300, 300)
    await app.mouse.move(320, 320)
    await expect(tool).toHaveCSS('background-color', HIGHLIGHT)

    await app.keyboard.up('Control')
    await expect(tool).not.toHaveCSS('background-color', HIGHLIGHT)
  })

  test('ends with the next mouse move when the browser lost the keyup', async ({ app }) => {
    await openAddress(app)
    const tool = app.getByTestId('gs-toolbar-select')

    // A keydown no key is behind: the app believes Ctrl is held, no keyup will
    // ever come, and mouse events report it as up. That is the stuck state.
    await app.evaluate(() =>
      document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Control', ctrlKey: true, bubbles: true }))
    )
    await expect(tool).toHaveCSS('background-color', HIGHLIGHT)

    await app.mouse.move(300, 300)
    await expect(tool).not.toHaveCSS('background-color', HIGHLIGHT)
  })
})
