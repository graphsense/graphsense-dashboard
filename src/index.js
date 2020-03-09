import 'datatables.net-scroller-dt/css/scroller.dataTables.css'
import 'datatables.net-dt/css/jquery.dataTables.css'
import '@fortawesome/fontawesome-free/css/all.css'
import '@fortawesome/fontawesome-free/js/all.js'
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
  const de = numeral.localeData(locale)
  de.delimiters.thousands = '.'
}
moment.locale(locale)

const timezone = jstz.determine().name()
moment.tz.setDefault(timezone)

const model = new Start(locale)
model.render(document.body)
