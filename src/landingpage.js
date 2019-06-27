import statistics from './pages/statistics.html'
import header from './pages/static/boldheader.html'
import footer from './pages/static/footer.html'
import stats from './pages/stats.html'
import currency from './pages/currency.html'
import Component from './component.js'
import moment from 'moment'
import numeral from 'numeral'
import {replace} from './template_utils'
import {currencies} from './globals.js'

export default class Landingpage extends Component {
  constructor (dispatcher, search, keyspaces) {
    super()
    this.dispatcher = dispatcher
    this.stats = {}
    this.search = search
    this.keyspaces = keyspaces
    keyspaces.forEach(key => {
      this.stats[key] = this.stats[key]
    })
  }
  setStats (stats) {
    this.keyspaces.forEach(key => {
      this.stats[key] = stats[key]
    })
    this.setUpdate('stats')
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate(true)) {
      document.body.style.overflow = 'hidden scroll'
      this.root.innerHTML =
        '<div class="' + STATICPAGE_CLASSES + '">' + // eslint-disable-line no-undef
        header + statistics + replace(footer, {version: VERSION}) + // eslint-disable-line no-undef
        '</div>'
      for (let key in this.stats) {
        this.stats[key] = 'loading'
      }
      this.renderStats()
      let searchRoot = this.root.querySelector('.splash .search')
      this.search.setUpdate(true)
      this.search.render(searchRoot)
    } else if (this.shouldUpdate('stats')) {
      this.renderStats()
    } else {
      this.search.render()
    }
    super.render()
    return this.root
  }
  renderStats () {
    const imageContext = require.context('./style/img/currencies/', false)
    let currenciesEl = document.querySelector('#currencies')
    currenciesEl.innerHTML = ''
    Object.keys(this.stats).forEach((keyspace) => {
      let imageUrl = ''
      try {
        imageUrl = imageContext(`./${keyspace}.svg`)
      } catch (e) {
        console.error(e.message)
      }
      let s = this.stats[keyspace]
      let statistics = ''
      if (!s) {
        statistics = '<div class="text-grey">Not available</div>'
      } else if (s === 'loading') {
        statistics = '<div class="text-grey">Loading ...</div>'
      } else if (s === 'coming') {
        statistics = '<div class="coming">Coming soon!</div>'
      } else {
        let format = '0,000,000'
        let t = moment.unix(s.timestamp)
        let flat =
          { lastUpdate: (t.format('L') + ' ' + t.format('LT')).replace(/ /g, '&nbsp;'),
            latestBlock: s.no_blocks - 1,
            noAddresses: numeral(s.no_addresses).format(format),
            noClusters: numeral(s.no_clusters).format(format),
            noTransactions: numeral(s.no_transactions).format(format)
          }
        statistics = replace(stats, flat)
      }
      currenciesEl.innerHTML += replace(currency, {
        currency: currencies[keyspace],
        imageUrl,
        statistics
      })
    })
  }
}
