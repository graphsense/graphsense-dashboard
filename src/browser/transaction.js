import transaction from './transaction.html'
import moment from 'moment'
import {replace} from '../template_utils'
import BrowserComponent from './component.js'

export default class Transaction extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = transaction
    this.options =
      [
        {icon: 'sign-in-alt', optionText: 'Incoming addresses', message: 'initTxInputsTable'},
        {icon: 'sign-out-alt', optionText: 'Outgoing addresses', message: 'initTxOutputsTable'}
      ]
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    let flat = {
      txHash: this.data.txHash.substring(0, 32) + '...',
      timestamp: moment.unix(this.data.timestamp).fromNow(),
      totalInput: this.formatCurrency(this.data.totalInput[this.currency], this.data.keyspace),
      totalOutput: this.formatCurrency(this.data.totalOutput[this.currency], this.data.keyspace)
    }
    this.root.innerHTML = replace(this.template, {...this.data, ...flat})
    return this.root
  }
  requestData () {
    return {...super.requestData(), id: this.data.address, type: 'address'}
  }
}
