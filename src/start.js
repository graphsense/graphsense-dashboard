import Callable from './callable.js'
import Rest from './rest.js'
import Search from './search/search.js'
import Landingpage from './landingpage.js'
import Logger from './logger.js'
import actions from './actions/start.js'
import Login from './login/login.js'
import ReportLogger from './reportLogger.js'

const logger = Logger.create('Start') // eslint-disable-line no-unused-vars
const baseUrl = REST_ENDPOINT // eslint-disable-line no-undef

const prefixLength = 5

export default class Start extends Callable {
  constructor (locale) {
    super()
    this.isStart = true
    this.locale = locale
    this.rest = new Rest(baseUrl, prefixLength)
    this.search = new Search(this.call, ['addresses', 'transactions', 'labels', 'blocks'], 'search')
    this.login = new Login(this.call)
    this.reportLogger = new ReportLogger()
    this.landingpage = new Landingpage(this.call)
    this.landingpage.setLogin(this.login)
    this.registerDispatchEvents(actions)
    this.call('stats')
    this.call('changeLocale', locale)
    this.mapResult(this.rest.login(), 'loginResult')
    this.showLandingpage = true
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.app) {
      // let app do the rendering now
      return this.app.render(this.root)
    }
    return this.landingpage.render(this.root)
  }

  replay () {
    this.rest.disable()
    logger.debug('replay')
    this.isReplaying = true
    this.dispatcher.replay()
    this.isReplaying = false
    this.rest.enable()
  }
}
