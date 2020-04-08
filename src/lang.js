let languagePack = null

export const setLanguagePack = (pack) => {
  languagePack = pack
}

export const t = (key) => {
  if (!languagePack) return key
  return languagePack[key]
}

export const tt = (template) => {
  return template.replace(new RegExp('{{t:([^}]+)}}', 'g'), (match, key) => t(key))
}
