const terms = require('./pages/static/terms.html')
const privacy = require('./pages/static/privacy.html')
const about = require('./pages/static/about.html')
const slimheader = require('./pages/static/slimheader.html')
const boldheader = require('./pages/static/boldheader.html')
const officialpage = require('./pages/static/officialpage.html')
const utils = require('./template_utils.js')

module.exports = function render (locals) {
  let useslimheader = true
  switch (locals.path) {
    case '/terms.html' :
      locals.page = terms
      locals.title = 'Terms'
      break
    case '/privacy.html' :
      locals.page = privacy
      locals.title = 'Privacy'
      break
    case '/about.html' :
      locals.page = about
      locals.title = 'About'
      break
    case '/officialpage.html' :
      locals.page = officialpage
      locals.title = 'Graphsense'
      useslimheader = false
      break
  }
  locals.page = `<div class="container mx-auto px-4 flex-grow mt-8">${locals.page}</div>`
  if (useslimheader) {
    locals.header = utils.replace(slimheader, {title: locals.title})
  } else {
    locals.header = utils.replace(boldheader, {action: ''}) // put HTML for demo button etc here
  }
  const assets = Object.keys(locals.webpackStats.compilation.assets)
  const css = assets.filter(value => value.match(/\.css\?/)).map(file => { return {file} })
  let options = {
    htmlWebpackPlugin: {
      options: locals
    },
    css: css
  }
  return locals.template(options)
}
