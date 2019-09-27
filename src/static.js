const terms = require('./pages/static/terms.html')
const privacy = require('./pages/static/privacy.html')
const about = require('./pages/static/about.html')
const slimheader = require('./pages/static/slimheader.html')
const boldheader = require('./pages/static/boldheader.html')
const officialpage = require('./pages/static/officialpage.hbs')
const utils = require('./template_utils.js')
const request = __non_webpack_require__('request') // eslint-disable-line no-undef
const statsHtml = require('./pages/statsHtml.js')

const wrapPage = (page) => `<div class="container mx-auto px-4 flex-grow mt-8">${page}</div>`

module.exports = function render (locals, callback) {
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
      locals.title = 'Graphsense'
      useslimheader = false
      break
  }
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
  if (locals.path === '/officialpage.html') {
    let requestOptions = {
      url: 'https://api.graphsense.info/stats',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Authorization': 'Bearer ' + locals.token // eslint-disable-line no-undef
      }
    }
    request(requestOptions, (err, res, body) => {
      if (err) { return console.log(err) }

      try {
        body = JSON.parse(body)
      } catch (e) {
        return callback(new Error('got invalid json from stats'))
      }
      let stats = statsHtml.statsHtml(body)
      locals.page = wrapPage(officialpage({stats}))
      callback(null, locals.template(options))
    })
  } else {
    locals.page = wrapPage(locals.page)
    callback(null, locals.template(options))
  }
}
