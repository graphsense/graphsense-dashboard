import { test, expect, waitForPathfinder } from './fixtures'

/**
 * The standing guard for the things that only break in a real browser.
 *
 * `CLAUDE.md` documents four ways the elm-safe-virtual-dom patches have fallen
 * out of the build silently; without them the app throws
 * "Node.removeChild: Argument 1 is not an object" the first time a keyed list
 * changes length. And the `exportGraphics` port that shipped in 26.08.0 threw
 * during startup wiring, which killed every subscription registered after it —
 * file open, plugin ports, settings persistence — with nothing failing loudly.
 *
 * Both are invisible to `elm-test`. Both are a page load away from obvious.
 */
test.describe('the app boots', () => {
  test('without console errors or uncaught exceptions', async ({ app, problems }) => {
    await app.goto('/pathfinder')
    await waitForPathfinder(app)

    expect(problems).toEqual([])
  })

  test('with the safe virtual dom actually installed', async ({ app }) => {
    await app.goto('/pathfinder')
    await waitForPathfinder(app)

    // `elmTree` is set only by the patched elm/virtual-dom. main.js warns when
    // it is missing; asserting it here turns that warning into a failed build.
    const patched = await app.evaluate(() => Boolean(document.body.elmTree))
    expect(
      patched,
      'document.body.elmTree is missing: the build resolved the unpatched ' +
        'elm/virtual-dom. Check ELM_HOME and `make check-virtual-dom-fix`.'
    ).toBe(true)
  })

  test('and renders the graph and its toolbar', async ({ app }) => {
    await app.goto('/pathfinder')

    await expect(app.locator('#graph')).toBeVisible()
    await expect(app.getByTestId('gs-toolbar-save')).toBeVisible()
    await expect(app.getByTestId('gs-toolbar-open')).toBeVisible()
  })

  test('on the landing page too', async ({ app, problems }) => {
    await app.goto('/')

    // Wait on something the app renders rather than on networkidle: the latter
    // is a timing heuristic, and it is the classic way a suite like this starts
    // failing once in twenty runs on a slower machine.
    await expect(app.getByTestId('gs-landing-search')).toBeVisible()

    expect(problems).toEqual([])
  })
})
