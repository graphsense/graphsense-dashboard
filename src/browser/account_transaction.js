import { tt } from '../lang.js'
import transaction from './account_transaction.html'
import { replace } from '../template_utils'
import BrowserComponent from './component.js'

export default class AccountTransaction extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = transaction
    this.options = []
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    const flat = {
      tx_hash: this.data.tx_hash,
      timestamp: this.formatTimestampWithAgo(this.data.timestamp),
      value: this.formatCurrency(this.data.values, this.data.keyspace),
      height: this.data.height
    }
    this.root.innerHTML = replace(tt(this.template), { ...this.data, ...flat })
    this.renderInlineOptions()
    return this.root
  }

  requestData () {
    return { ...super.requestData(), id: this.data.address, type: 'address' }
  }
}
