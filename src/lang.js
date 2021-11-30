import { firstToUpper } from './utils.js'
import { esc } from './template_utils.js'
let languagePack = null

export const setLanguagePack = (pack) => {
  languagePack = pack
}

export const t = (key, ...args) => {
  const lk = key.toLowerCase()
  if (!languagePack || !languagePack[lk]) return key
  const str = languagePack[lk].replace(new RegExp('%([0-9]+)', 'g'), (match, key) => args[key * 1] === undefined || args[key * 1] === null ? '' : args[key * 1])
  if (lk[0] === key[0]) return str
  // ie. first character is upper case so make the result also first char uppercase
  return firstToUpper(str)
}

export const tt = (template) => {
  const id = s => s
  return template.replace(new RegExp('{{t(e?):([^}]+)}}', 'g'), (match, e, key) => (e ? esc : id)(t(key)))
}

export let dtLanguagePack = null

export const setDTLanguagePack = (pack) => {
  dtLanguagePack = pack
}
