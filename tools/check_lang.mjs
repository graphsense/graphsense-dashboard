#!/usr/bin/env node
//
// Guards the translation files against drift.
//
// The reference key set is the union of
//   * every key in lang/en.yaml  (which is an *override* map — English text
//     that differs from the key itself, e.g. "entity: cluster"), and
//   * every literal key passed to a View.Locale lookup in src/.
//
// View.Locale.string falls back to the key itself when a lookup misses, so a
// gap in en.yaml is harmless (the key *is* the English text) but a gap in any
// other locale silently shows English to that user.
//
// Known gaps are recorded in lang/untranslated-baseline.json, in the spirit of
// review/suppressed/: everything already missing today is tolerated, anything
// new fails. The baseline is also checked for entries that have since been
// translated, so it shrinks instead of rotting.
//
// Usage:
//   node tools/check_lang.mjs                      check (used by `make check-lang`)
//   node tools/check_lang.mjs --update-baseline    accept the current gaps
//   node tools/check_lang.mjs --no-baseline        report every gap, ignore the baseline
//   node tools/check_lang.mjs --verbose            list stale keys instead of counting them
//
// A plugin keeps its own lang/ and src/ in its own repository, so it points the
// same check at those instead of the defaults:
//
//   node ../graphsense-dashboard/tools/check_lang.mjs --lang-dir lang --src src
//
// The plugin's en.yaml is an override map exactly like core's, and its locales
// are checked against the same union of en.yaml keys and literal keys in src/.
// --baseline is optional there: with no such file, every gap is an error, which
// is the right default for a lang set that starts complete.

import fs from 'fs'
import path from 'path'
import YAML from 'yaml'

// `--flag value`, or the default when it is absent. Repeatable flags collect.
function option (flag, fallback) {
  const values = process.argv.flatMap((arg, i) =>
    arg === flag && process.argv[i + 1] ? [process.argv[i + 1]] : []
  )
  return values.length ? values : fallback
}

const LANG_DIR = option('--lang-dir', ['lang'])[0]
const REFERENCE = 'en'
const SRC_DIRS = option('--src', ['src'])
const BASELINE_FILE = option('--baseline', [path.join(LANG_DIR, 'untranslated-baseline.json')])[0]

// View.Locale functions whose second argument is a translation key.
const LOOKUP_FUNCTIONS = [
  'string',
  'text',
  'capitalized',
  'title',
  'titleCase',
  'markdown',
  'interpolated',
  'interpolatedMarkdown'
]

// `Locale.string vc.locale "some key"` — the model argument is a plain (dotted)
// identifier at every call site we can check statically.
//
// Matched against the whole file rather than line by line: elm-format breaks a
// call whose key is long, and
//
//     Locale.interpolated vc.locale
//         "{0} addresses were found"
//         [ ... ]
//
// is the normal shape for every interpolated string in the codebase. Read a line
// at a time those were invisible — not reported as computed, just missed, which
// is the one failure mode this check must not have.
const LOOKUP_RE = new RegExp(
  `Locale\\.(${LOOKUP_FUNCTIONS.join('|')})\\s+[A-Za-z_][A-Za-z0-9_.]*\\s+"((?:[^"\\\\]|\\\\.)*)"`,
  'g'
)

// Same call, but with a computed key — counted so the summary is honest about
// what this check cannot see.
const COMPUTED_KEY_RE = new RegExp(
  `Locale\\.(${LOOKUP_FUNCTIONS.join('|')})\\s+[A-Za-z_][A-Za-z0-9_.]*\\s+\\(`,
  'g'
)

// View.Locale.string lowercases the first character before the lookup, so
// "Align horizontally" and "align horizontally" are the same entry.
const normalize = (key) => key.slice(0, 1).toLowerCase() + key.slice(1)

function readLocales () {
  const locales = {}
  for (const file of fs.readdirSync(LANG_DIR).sort()) {
    if (!/\.ya?ml$/.test(file)) continue
    const parsed = YAML.parse(fs.readFileSync(path.join(LANG_DIR, file), 'utf8'))
    if (parsed === null || typeof parsed !== 'object' || Array.isArray(parsed)) {
      throw new Error(`${LANG_DIR}/${file} is not a key/value mapping`)
    }
    locales[file.replace(/\.ya?ml$/, '')] = new Set(Object.keys(parsed).map(normalize))
  }
  return locales
}

function elmFiles (dir) {
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name)
    if (entry.isDirectory()) return elmFiles(full)
    return entry.isFile() && entry.name.endsWith('.elm') ? [full] : []
  })
}

// Map<normalized key, ["file:line", ...]>, plus a count of computed call sites.
function collectUsedKeys () {
  const used = new Map()
  let computed = 0

  for (const dir of SRC_DIRS) {
    for (const file of elmFiles(dir)) {
      // Commented-out lines are blanked rather than dropped, so the offsets a
      // whole-file match reports still map back to the right line number.
      const lines = fs.readFileSync(file, 'utf8').split('\n')
      const text = lines.map((l) => (l.trimStart().startsWith('--') ? '' : l)).join('\n')
      const lineOf = (offset) => text.slice(0, offset).split('\n').length

      for (const match of text.matchAll(LOOKUP_RE)) {
        const key = normalize(match[2])
        if (!used.has(key)) used.set(key, [])
        used.get(key).push(`${file}:${lineOf(match.index)}`)
      }
      computed += [...text.matchAll(COMPUTED_KEY_RE)].length
    }
  }
  return { used, computed }
}

function readBaseline () {
  if (!fs.existsSync(BASELINE_FILE)) return {}
  const raw = JSON.parse(fs.readFileSync(BASELINE_FILE, 'utf8'))
  const baseline = {}
  for (const [locale, keys] of Object.entries(raw)) {
    if (locale.startsWith('_')) continue // comment fields
    baseline[locale] = new Set(keys.map(normalize))
  }
  return baseline
}

function writeBaseline (gaps) {
  const out = {
    _comment: [
      'Translation keys known to be missing per locale. Generated by',
      '`node tools/check_lang.mjs --update-baseline`. Shrink it by translating',
      'the keys listed here; anything not listed fails `make check-lang`.'
    ].join(' ')
  }
  for (const locale of Object.keys(gaps).sort()) {
    if (gaps[locale].length) out[locale] = [...gaps[locale]].sort()
  }
  fs.writeFileSync(BASELINE_FILE, JSON.stringify(out, null, 2) + '\n')
}

function main () {
  const updateBaseline = process.argv.includes('--update-baseline')
  const ignoreBaseline = process.argv.includes('--no-baseline')
  const verbose = process.argv.includes('--verbose')

  const locales = readLocales()
  if (!locales[REFERENCE]) {
    console.error(`error: ${LANG_DIR}/${REFERENCE}.yaml not found`)
    process.exit(1)
  }

  const { used, computed } = collectUsedKeys()
  const reference = new Set([...locales[REFERENCE], ...used.keys()])
  const baseline = ignoreBaseline ? {} : readBaseline()

  const gaps = {} // locale -> all missing keys (for --update-baseline)
  let errors = 0
  let warnings = 0

  for (const [locale, keys] of Object.entries(locales)) {
    // en.yaml is the reference: a key it lacks falls back to itself, which is
    // exactly the English text. Only the other locales can have a real gap.
    const missing =
      locale === REFERENCE ? [] : [...reference].filter((k) => !keys.has(k)).sort()
    gaps[locale] = missing

    const suppressed = baseline[locale] || new Set()
    const unexpected = missing.filter((k) => !suppressed.has(k))
    const fixed = [...suppressed].filter((k) => keys.has(k)).sort()
    const stale = [...keys].filter((k) => !reference.has(k)).sort()

    if (unexpected.length) {
      errors += unexpected.length
      console.error(`\n${LANG_DIR}/${locale}.yaml is missing ${unexpected.length} key(s):`)
      unexpected.forEach((k) => {
        console.error(`  - ${JSON.stringify(k)}`)
        ;(used.get(k) || []).slice(0, 2).forEach((loc) => console.error(`      ${loc}`))
      })
    }
    if (fixed.length) {
      errors += fixed.length
      console.error(
        `\n${LANG_DIR}/${locale}.yaml now translates ${fixed.length} key(s) still listed in ${BASELINE_FILE}:`
      )
      fixed.forEach((k) => console.error(`  - ${JSON.stringify(k)}`))
    }
    if (stale.length) {
      warnings += stale.length
      if (verbose || updateBaseline) {
        console.error(
          `\nwarning: ${LANG_DIR}/${locale}.yaml has ${stale.length} key(s) no longer in the reference set:`
        )
        stale.forEach((k) => console.error(`  - ${JSON.stringify(k)}`))
      } else {
        console.error(
          `warning: ${LANG_DIR}/${locale}.yaml has ${stale.length} key(s) no longer in the reference set (--verbose to list)`
        )
      }
    }
  }

  if (updateBaseline) {
    writeBaseline(gaps)
    console.log(`wrote ${BASELINE_FILE}`)
    return
  }

  const summary =
    `${Object.keys(locales).length} locale(s), ` +
    `${reference.size} reference key(s) ` +
    `(${locales[REFERENCE].size} from ${REFERENCE}.yaml, ${used.size} literal key(s) in ${SRC_DIRS.join(', ')}/, ` +
    `${computed} computed call site(s) skipped)`

  if (errors) {
    console.error(
      `\ncheck-lang FAILED: ${errors} error(s), ${warnings} warning(s) — ${summary}\n` +
        `If the gap is intentional for now, run: node tools/check_lang.mjs --update-baseline`
    )
    process.exit(1)
  }
  console.log(`check-lang OK: ${warnings} warning(s) — ${summary}`)
}

main()
