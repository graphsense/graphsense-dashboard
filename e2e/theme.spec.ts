import { test, expect, waitForPathfinder } from './fixtures'

/**
 * System / Light / Dark. What `elm-test` cannot see: the `prefers-color-scheme`
 * flag and its change port in `src/main.js`, and the script in `index.html`
 * that darkens the page before Elm starts.
 */

const colorScheme = (page: import('@playwright/test').Page) =>
  page.evaluate(() => getComputedStyle(document.documentElement).colorScheme)

const boot = async (page: import('@playwright/test').Page, stored: Record<string, string> = {}) => {
  await page.addInitScript((s) => {
    for (const [k, v] of Object.entries(s)) localStorage.setItem(k, v)
  }, stored)
  await page.goto('/pathfinder')
  await waitForPathfinder(page)
}

test.describe('the colour scheme', () => {
  test('follows the system by default, and live', async ({ app, problems }) => {
    await app.emulateMedia({ colorScheme: 'dark' })
    await boot(app)
    await expect.poll(() => colorScheme(app)).toBe('dark')

    await app.emulateMedia({ colorScheme: 'light' })
    await expect.poll(() => colorScheme(app)).toBe('light')

    expect(problems).toEqual([])
  })

  test('an explicit Light ignores a dark system', async ({ app }) => {
    await app.emulateMedia({ colorScheme: 'dark' })
    await boot(app, { selectedLanguage: 'en', themeMode: 'light' })
    await expect.poll(() => colorScheme(app)).toBe('light')
  })

  test('an explicit Dark ignores a light system, also when it changes', async ({ app }) => {
    await app.emulateMedia({ colorScheme: 'light' })
    await boot(app, { selectedLanguage: 'en', themeMode: 'dark' })
    await expect.poll(() => colorScheme(app)).toBe('dark')

    await app.emulateMedia({ colorScheme: 'dark' })
    await app.emulateMedia({ colorScheme: 'light' })
    await expect.poll(() => colorScheme(app)).toBe('dark')
  })

  test('the old lightMode=true moves to System', async ({ app }) => {
    await app.emulateMedia({ colorScheme: 'dark' })
    await boot(app, { selectedLanguage: 'en', lightMode: 'true' })
    await expect.poll(() => colorScheme(app)).toBe('dark')
  })

  test('is dark before Elm renders, so a reload does not flash white', async ({ app }) => {
    await app.emulateMedia({ colorScheme: 'light' })
    await app.addInitScript(() => {
      localStorage.setItem('selectedLanguage', 'en')
      localStorage.setItem('themeMode', 'dark')
      // Record what the page looks like the moment <body> exists, before
      // main.js (a module script, so deferred) has started Elm.
      document.addEventListener('readystatechange', () => {
        if (document.readyState === 'interactive' && !(window as any).__schemeAtBoot) {
          ;(window as any).__schemeAtBoot = getComputedStyle(document.documentElement).colorScheme
        }
      })
      new MutationObserver((_, obs) => {
        if (document.body) {
          ;(window as any).__schemeAtBoot = getComputedStyle(document.documentElement).colorScheme
          obs.disconnect()
        }
      }).observe(document.documentElement, { childList: true })
    })
    await app.goto('/pathfinder')
    await waitForPathfinder(app)
    expect(await app.evaluate(() => (window as any).__schemeAtBoot)).toBe('dark')
    // and once Elm runs, the boot marker is gone so it cannot pin the scheme
    expect(await app.evaluate(() => document.documentElement.dataset.theme)).toBeUndefined()
  })
})
