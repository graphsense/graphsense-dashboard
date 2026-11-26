import { test, expect, waitForPathfinder, ADDRESS, addressNode } from './fixtures'

const GRAPH_ID = 'graph'

/**
 * The overview map turns real mouse positions (offsetX/offsetY on the map)
 * into graph coordinates, and follows a drag only while the button is held.
 * Neither can be checked without a browser.
 */

test.describe('overview map', () => {
  test('appears when the graph leaves the screen, and a drag on it brings the graph back', async ({ app }) => {
    await app.goto(`/pathfinder/btc/address/${ADDRESS}`)
    await waitForPathfinder(app)
    const node = addressNode(app, ADDRESS)
    await expect(node).toBeVisible()
    await expect(app.getByTestId('gs-overview')).toHaveCount(0)

    // zoom in far from the address until it is off screen
    await app.mouse.move(250, 250)
    for (let i = 0; i < 6; i++) {
      await app.mouse.wheel(0, -400)
      await app.waitForTimeout(100)
    }
    await expect(node).not.toBeInViewport()
    const map = app.getByTestId('gs-overview-map')
    await expect(map).toBeVisible()

    // the view follows the pointer while the button is down: press on one
    // edge of the map, then drag to its middle, where the single address is
    const box = await map.boundingBox()
    if (!box) throw new Error('the overview map has no box')
    const viewBox = () => app.locator(`svg#${GRAPH_ID}`).getAttribute('viewBox')
    await app.mouse.move(box.x + 5, box.y + box.height / 2)
    await app.mouse.down()
    await app.waitForTimeout(600) // let the view settle on the pressed point
    const pressed = await viewBox()
    await app.mouse.move(box.x + box.width / 2, box.y + box.height / 2, { steps: 5 })
    await app.waitForTimeout(600)
    const dragged = await viewBox()
    await app.mouse.up()

    expect(dragged).not.toEqual(pressed)
    await expect(node).toBeInViewport()
  })

  test('the – hides it and "Show overview" brings it back', async ({ app }) => {
    await app.goto(`/pathfinder/btc/address/${ADDRESS}`)
    await waitForPathfinder(app)
    await app.mouse.move(250, 250)
    for (let i = 0; i < 6; i++) {
      await app.mouse.wheel(0, -400)
      await app.waitForTimeout(100)
    }
    await expect(app.getByTestId('gs-overview')).toBeVisible()

    await app.getByTestId('gs-overview-hide').click()
    await expect(app.getByTestId('gs-overview')).toHaveCount(0)

    await app.getByTestId('gs-overview-show').click()
    await expect(app.getByTestId('gs-overview')).toBeVisible()
  })
})
