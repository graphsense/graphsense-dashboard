import Callable from './callable.js'
import Rest from './rest.js'
import Search from './search/search.js'
import Landingpage from './landingpage.js'
import Logger from './logger.js'
import actions from './actions/start.js'

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
    this.landingpage = new Landingpage(this.call, this.search, this.keyspaces)
    this.registerDispatchEvents(actions)
    this.call('stats')
    import('./model.js').then(model => { // works despite of parsing error of eslint
      this.model = new model.default(this.locale, this.search, this.landingpage)
    })
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if(this.model) {
      // let model do the rendering
      return this.model.render(this.root)
    }
    return this.landingpage.render(this.root)
  }
}
