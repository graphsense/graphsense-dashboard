import { test, expect, waitForPathfinder } from './fixtures'

/**
 * The placeholder is put together from translated words, and the English
 * texts come from lang/en.yaml over HTTP at runtime, which elm-test never
 * loads. "actor" was mapped to "Actor", so the English placeholder read
 * "Address, transaction, Actor".
 */

test('the search placeholder lists its kinds in lower case after the first', async ({ app }) => {
  await app.goto('/pathfinder')
  await waitForPathfinder(app)
  const input = app.locator('#search_input').first()
  await expect(input).toHaveAttribute('placeholder', /^Address, transaction, actor\b/)
})
