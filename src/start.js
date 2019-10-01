import Callable from './callable.js'
import Rest from './rest.js'
import Search from './search/search.js'
import Landingpage from './landingpage.js'
import Logger from './logger.js'
import actions from './actions/start.js'
import Login from './login/login.js'

const logger = Logger.create('Start') // eslint-disable-line no-unused-vars
const baseUrl = REST_ENDPOINT // eslint-disable-line no-undef

// TODO code duplication!
let supportedKeyspaces

try {
  supportedKeyspaces = JSON.parse(SUPPORTED_KEYSPACES) // eslint-disable-line no-undef
  if (!Array.isArray(supportedKeyspaces)) throw new Error('SUPPORTED_KEYSPACES is not an array')
} catch (e) {
  console.error(e.message)
  supportedKeyspaces = []
}

const prefixLength = 5

export default class Start extends Callable {
  constructor (locale) {
    super()
    this.locale = locale
    this.rest = new Rest(baseUrl, prefixLength)
    this.keyspaces = supportedKeyspaces
    this.search = new Search(this.call, this.keyspaces)
    this.login = new Login(this.call)
    this.landingpage = new Landingpage(this.call, this.keyspaces)
    this.landingpage.setLogin(this.login)
    this.registerDispatchEvents(actions)
    this.call('stats')
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
}
