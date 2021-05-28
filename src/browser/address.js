import address from './address.html'
import { replace } from '../template_utils'
import BrowserComponent from './component.js'
import { t, tt } from '../lang.js'
import numeral from 'numeral'

export default class Address extends BrowserComponent {
  constructor (dispatcher, data, index, currency, categories) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = address
    this.categories = categories
    this.options =
      [
        { inline: 'row-incoming', optionText: 'Sending addresses', message: 'initIndegreeTable' },
        { inline: 'row-outgoing', optionText: 'Receiving addresses', message: 'initOutdegreeTable' },
        { inline: 'row-transactions', optionText: 'Transactions', message: 'initTransactionsTable' },
        { inline: 'row-tags', optionText: 'Tags', message: 'initTagsTable' }
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
    this.renderInlineOptions()
    return this.root
  }

  flattenData () {
    let timestamps = this.data.map(d => d.first_tx.timestamp)
    const first = Math.min(...timestamps)
    timestamps = this.data.map(d => d.last_tx.timestamp)
    const last = Math.max(...timestamps)
    const duration = (last - first) * 1000
    const tags = this.flattenTags()
    const abuses = [...new Set(tags.filter(({ abuse }) => abuse).map(({ abuse }) => this.categories[abuse] ? this.categories[abuse].label : '').filter(a => a).values())]
    const categories = [...new Set(tags.filter(({ category }) => category).map(({ category }) => this.categories[category] ? this.categories[category].label : '').filter(a => a).values())]
    const totalReceived = this.data.reduce((sum, v) => sum + v.total_received[this.currency], 0)
    const balance = this.data.reduce((sum, v) => sum + v.balance[this.currency], 0)
    const noOutgoingTxs = this.data.reduce((sum, v) => sum + v.no_outgoing_txs, 0)
    const noIncomingTxs = this.data.reduce((sum, v) => sum + v.no_incoming_txs, 0)
    const noOutdegree = this.data.reduce((sum, v) => sum + v.out_degree, 0)
    const noIndegree = this.data.reduce((sum, v) => sum + v.in_degree, 0)
    const tagCoherence = this.data.length === 1 && this.data[0].tag_coherence !== null ? numeral(this.data[0].tag_coherence).format('0.[00]%') : t('unknown')
    const keyspace = [...new Set(this.data.map(d => d.keyspace.toUpperCase()))].join(' ')
    const esc = s => s.replace(' ', '&nbsp;')
    return {
      id: '<div>' + this.data.map(d => d.id).join('</div><div>') + '</div>',
      first_usage: esc(this.formatTimestampWithAgo(first)),
      last_usage: esc(this.formatTimestampWithAgo(last)),
      activity_period: esc(this.formatDuration(duration)),
      total_received: esc(this.formatCurrency(totalReceived, keyspace)),
      balance: esc(this.formatCurrency(balance, keyspace)),
      keyspace,
      abuses: esc(abuses.join(', ')),
      categories: esc(categories.join(', ')),
      no_outgoing_txs: esc(numeral(noOutgoingTxs).format('0,000')),
      no_incoming_txs: esc(numeral(noIncomingTxs).format('0,000')),
      no_transfers: esc(numeral(noIncomingTxs + noOutgoingTxs).format('0,000')),
      out_degree: esc(numeral(noOutdegree).format('0,000')),
      in_degree: esc(numeral(noIndegree).format('0,000')),
      no_tags: esc(numeral(tags.length).format('0,000')),
      tagCoherence,
      label_transfers: esc(t('Transfers') + keyspace === 'eth' ? ' (ext.)' : ''),
      label_activity_period: esc(t('Activity period')),
      label_receiving_addresses: esc(t('Receiving addresses')),
      label_sending_addresses: esc(t('Sending addresses')),
      label_first_usage: esc(t('First usage')),
      label_last_usage: esc(t('Last usage')),
      label_total_received: esc(t('Total received')),
      label_final_balance: esc(t('Final balance'))
    }
  }

  flattenTags () {
    return this.data.reduce((tags, d) => tags.concat(d.tags || []), [])
  }

  requestData () {
    return { ...super.requestData(), id: this.data[0].id, type: this.data[0].type }
  }
}
