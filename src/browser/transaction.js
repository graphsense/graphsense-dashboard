import transaction from './transaction.html'
import moment from 'moment'
import {replace} from '../template_utils'
import BrowserComponent from './component.js'
import {formatCurrency} from '../utils'

export default class Transaction extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = transaction
    this.options =
      [
      ]
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    let flat = {
      txHash: this.data.txHash.substring(0, 32) + '...',
      timestamp: moment.unix(this.data.timestamp).fromNow(),
      totalInput: this.formatCurrency(this.data.totalInput[this.currency]),
      totalOutput: this.formatCurrency(this.data.totalOutput[this.currency])
    }
    this.root.innerHTML = replace(this.template, {...this.data, ...flat})
    return this.root
  }
  requestData () {
    return {id: this.data.address, type: 'address', index: this.index}
  }
}
