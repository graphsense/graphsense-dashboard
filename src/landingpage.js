import layout from './layout/landingpage.html'
import stats from './layout/stats.html'
import Component from './component.js'
import moment from 'moment'
import numeral from 'numeral'
import {replace} from './template_utils'

export default class Landingpage extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
    this.stats = {}
  }
  setStats (keyspace, stats) {
    this.stats[keyspace] = stats
    this.shouldUpdate('stats')
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate() === true) {
      this.root.innerHTML = layout
      this.stats['btc'] = 'loading'
      this.stats['ltc'] = 'loading'
      this.stats['bch'] = 'coming'
      this.dispatcher('stats', 'btc')
      this.dispatcher('stats', 'ltc')
      this.renderStats()
    } else if (this.shouldUpdate() === 'stats') {
      this.renderStats()
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
        { lastUpdate: moment.unix(s.timestampLastBlock).fromNow(),
          latestBlock: s.noBlocks - 1,
          noAddresses: numeral(s.noAddresses).format(format),
          noClusters: numeral(s.noClusters).format(format),
          noTransactions: numeral(s.noTransactions).format(format)
        }
      el.innerHTML = replace(stats, flat)
    })
  }
}
