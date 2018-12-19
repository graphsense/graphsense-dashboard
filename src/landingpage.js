import statistics from './pages/statistics.html'
import header from './pages/static/boldheader.html'
import footer from './pages/static/footer.html'
import stats from './pages/stats.html'
import Component from './component.js'
import moment from 'moment'
import numeral from 'numeral'
import {replace} from './template_utils'

export default class Landingpage extends Component {
  constructor (dispatcher, search, keyspaces) {
    super()
    this.dispatcher = dispatcher
    this.stats = {}
    this.search = search
    this.stats = {...keyspaces}
  }
  setStats (stats) {
    this.stats = stats
    this.shouldUpdate('stats')
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate() === true) {
      this.root.innerHTML =
        '<div class="' + STATICPAGE_CLASSES + '">' //eslint-disable-line
        + header + statistics + replace(footer, {version: VERSION}) + //eslint-disable-line
        '</div>'
      for (let key in this.stats) {
        this.stats[key] = 'loading'
      }
      this.dispatcher('stats')
      this.renderStats()
      let searchRoot = this.root.querySelector('.splash .search')
      this.search.shouldUpdate(true)
      this.search.render(searchRoot)
    } else if (this.shouldUpdate() === 'stats') {
      this.renderStats()
    } else {
      this.search.render()
    }
    super.render()
    return this.root
  }
  renderStats () {
    Object.keys(this.stats).forEach((keyspace) => {
      let s = this.stats[keyspace]
      let el = this.root.querySelector(`.${keyspace} .statistics`)
      if (!el) return
      if (s === 'loading') {
        el.innerHTML = '<div class="text-grey">Loading ...</div>'
        return
      } else if (s === 'coming') {
        el.innerHTML = '<div class="coming">Coming soon!</div>'
        return
      }
      let format = '0,000,000'
      let flat =
        { lastUpdate: moment.unix(s.timestamp).fromNow(),
          latestBlock: s.no_blocks - 1,
          noAddresses: numeral(s.no_addresses).format(format),
          noClusters: numeral(s.no_clusters).format(format),
          noTransactions: numeral(s.no_transactions).format(format)
        }
      el.innerHTML = replace(stats, flat)
    })
  }
}
