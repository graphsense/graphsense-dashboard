import 'datatables.net-scroller-dt/css/scroller.dataTables.css'
import 'datatables.net-dt/css/jquery.dataTables.css'
import '@fortawesome/fontawesome-free/css/all.css'
import './style/Octarine-Bold/fonts.css'
import './style/Octarine-Light/fonts.css'
import 'd3-context-menu/css/d3-context-menu.css'
import './style/style.css'
import Start from './start.js'
import numeral from 'numeral'
import moment from 'moment'
import Logger from './logger.js'
import jstz from 'jstimezonedetect'
import 'moment-timezone'
import 'numeral/locales'

Logger.setLogLevel(IS_DEV ? Logger.LogLevels.DEBUG : Logger.LogLevels.ERROR) // eslint-disable-line no-undef

const getNavigatorLanguage = () => {
  if (navigator.languages && navigator.languages.length) {
    return navigator.languages[0]
  } else {
    return navigator.userLanguage || navigator.language || navigator.browserLanguage || 'en'
  }
}

const locale = getNavigatorLanguage().split('-')[0]
numeral.locale(locale)
try {
  numeral.localeData(locale)
} catch (e) {
  console.warn(`Couldn't find locale '${locale}', falling back to 'en'`)
  numeral.locale('en')
}

if (locale === 'de') {
  // overwrite locale format
  let de = numeral.localeData(locale)
  de.delimiters.thousands = '.'
}
moment.locale(locale)

const timezone = jstz.determine().name()
moment.tz.setDefault(timezone)

let model = new Start(locale)
model.render(document.body)

if (module.hot) {
  let Model
  /*
  import('./app.js').then(app => { // works despite of parsing error of eslint
    Model = app.default
    model = new Model(locale, model.rest, model.stats)
    model.render(document.body)
  })
  */
  module.hot.accept([
    './browser.js',
    './browser/address.html',
    './browser/address.js',
    './browser/addresses_table.js',
    './browser/entity.html',
    './browser/entity.js',
    './browser/component.js',
    './browser/layout.html',
    './browser/option.html',
    './search/search.html',
    './search/search.js',
    './login/login.html',
    './login/login.js',
    './status/status.html',
    './statusbar.js',
    './browser/table.html',
    './browser/table.js',
    './browser/tags_table.js',
    './browser/transaction.html',
    './browser/transaction.js',
    './browser/transaction_addresses_table.js',
    './browser/transactions_table.js',
    './nodeGraph.js',
    './nodeGraph/addressNode.js',
    './nodeGraph/entityNode.js',
    './nodeGraph/graphNode.js',
    './nodeGraph/layer.js',
    './config.js',
    './config/address.html',
    './config/entity.html',
    './config/filter.html',
    './config/graph.html',
    './config/layout.html',
    './layout.js',
    './layout/layout.html',
    './component.js',
    './app.js',
    './config.js',
    './start.js',
    './rest.js',
    './store.js',
    './template_utils.js',
    './utils.js'
  ], () => {
    // dispatcher.history = [debugHistory[0]]

    if (!Model) return
    model = new Model(locale)
    model.replay()
    model.render(document.body)
  })
  if ('serviceWorker' in navigator) {
    // navigator.serviceWorker.register('./sw.js')
  }
}
