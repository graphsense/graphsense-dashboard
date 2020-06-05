import address from './address.html'
import { replace } from '../template_utils'
import BrowserComponent from './component.js'
import incomingNeighbors from '../icons/incomingNeighbors.html'
import outgoingNeighbors from '../icons/outgoingNeighbors.html'
import { t, tt } from '../lang.js'
import numeral from 'numeral'

export default class Address extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = address
    this.options =
      [
        { html: incomingNeighbors, optionText: 'Incoming neighbors', message: 'initIndegreeTable' },
        { html: outgoingNeighbors, optionText: 'Outgoing neighbors', message: 'initOutdegreeTable' },
        { icon: 'exchange-alt', optionText: 'Transactions', message: 'initTransactionsTable' },
        { icon: 'tags', optionText: 'Tags', message: 'initTagsTable' }
      ]
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    this.options = this.data.length > 1 ? [] : this.options
    super.render()
    const flat = this.flattenData()
    this.root.innerHTML = replace(tt(this.template), flat)
    if (flat.abuses.length === 0) {
      const el = this.root.querySelector('#abuses')
      if (el) el.style.display = 'none'
    }
    if (flat.categories.length === 0) {
      const el = this.root.querySelector('#categories')
      if (el) el.style.display = 'none'
    }
    return this.root
  }

  flattenData () {
    let timestamps = this.data.map(d => d.first_tx.timestamp)
    const first = Math.min(...timestamps)
    timestamps = this.data.map(d => d.last_tx.timestamp)
    const last = Math.max(...timestamps)
    const duration = (last - first) * 1000
    const tags = this.data.reduce((tags, d) => tags.concat(d.tags || []), [])
    const abuses = [...new Set(tags.filter(({ abuse }) => abuse).map(({ abuse }) => abuse).values())]
    const categories = [...new Set(tags.filter(({ category }) => category).map(({ category }) => category).values())]
    const totalReceived = this.data.reduce((sum, v) => sum + v.total_received[this.currency], 0)
    const balance = this.data.reduce((sum, v) => sum + v.balance[this.currency], 0)
    const noOutgoingTxs = this.data.reduce((sum, v) => sum + v.no_outgoing_txs, 0)
    const noIncomingTxs = this.data.reduce((sum, v) => sum + v.no_incoming_txs, 0)
    const noOutdegree = this.data.reduce((sum, v) => sum + v.out_degree, 0)
    const noIndegree = this.data.reduce((sum, v) => sum + v.in_degree, 0)
    const reliability = this.data.length === 1 && this.data[0].reliability !== null ? numeral(this.data[0].reliability).format('0.[00]%') : t('unknown')
    const keyspace = [...new Set(this.data.map(d => d.keyspace.toUpperCase()))].join(' ')
    return {
      id: '<div>' + this.data.map(d => d.id).join('</div><div>') + '</div>',
      first_usage: this.formatTimestampWithAgo(first),
      last_usage: this.formatTimestampWithAgo(last),
      activity_period: this.formatDuration(duration),
      total_received: this.formatCurrency(totalReceived, keyspace),
      balance: this.formatCurrency(balance, keyspace),
      keyspace,
      abuses: abuses.join(' '),
      categories: categories.join(' '),
      no_outgoing_txs: noOutgoingTxs,
      no_incoming_txs: noIncomingTxs,
      out_degree: noOutdegree,
      in_degree: noIndegree,
      reliability
    }
  }

  requestData () {
    return { ...super.requestData(), id: this.data[0].id, type: this.data[0].type }
  }
}
