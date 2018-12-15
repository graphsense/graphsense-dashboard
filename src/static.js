const terms = require('./pages/terms.html')
const privacy = require('./pages/privacy.html')
const about = require('./pages/about.html')
const header = require('./pages/header.html')
const utils = require('./template_utils.js')

module.exports = function render (locals) {
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
  }
  locals.page = `<div class="container mx-auto px-4 flex-grow mt-8">${locals.page}</div>`
  console.log('title', locals.title)
  locals.header = utils.replace(header, {title: locals.title})
  let options = {
    htmlWebpackPlugin: {
      options: locals
    }
  }
  return locals.template(options)
}
