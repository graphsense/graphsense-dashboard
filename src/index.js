import 'datatables.net-scroller-dt/css/scroller.dataTables.css'
import 'datatables.net-dt/css/jquery.dataTables.css'
import '@fortawesome/fontawesome-free/css/all.css'
import '@fortawesome/fontawesome-free/js/all.js'
import './style/fonts/din-webfont/fonts.css'
import './style/fonts/Octarine-Light/fonts.css'
import './style/fonts/Octarine-Light/fonts.css'
import 'd3-context-menu/css/d3-context-menu.css'
import Start from './start.js'
import numeral from 'numeral'
import moment from 'moment'
import Logger from './logger.js'
import jstz from 'jstimezonedetect'
import 'moment-timezone'
import 'numeral/locales'
import './style/style.css'

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
  const de = numeral.localeData(locale)
  de.delimiters.thousands = '.'
}
moment.locale(locale)

const timezone = jstz.determine().name()
moment.tz.setDefault(timezone)

let model = new Start(locale)
model.render(document.body)

if (module.hot) {
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

    model = new Start(locale) // eslint-disable-line new-cap
    model.replay()
    model.render(document.body)
  })

  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('./sw.js')
  }
}
