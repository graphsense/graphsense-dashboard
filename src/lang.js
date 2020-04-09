import { firstToUpper } from './utils.js'
let languagePack = null

export const setLanguagePack = (pack) => {
  languagePack = pack
  console.log('language pack', pack)
}

export const t = (key, ...args) => {
  const lk = key.toLowerCase()
  if (!languagePack || !languagePack[lk]) return key
  const str = languagePack[lk].replace(new RegExp('%([0-9]+)', 'g'), (match, key) => args[key * 1] || '')
  console.log('lang key', key, str)
  if (lk[0] === key[0]) return str
  // ie. first character is upper case so make the result also first char uppercase
  return firstToUpper(str)
}

export const tt = (template) => {
  return template.replace(new RegExp('{{t:([^}]+)}}', 'g'), (match, key) => t(key))
}
