import transaction from './transaction.html'
import moment from 'moment'
import {replace} from '../template_utils'
import BrowserComponent from './component.js'

export default class Transaction extends BrowserComponent {
  constructor (dispatcher, data, index) {
    super(dispatcher, index)
    this.data = data
    this.template = transaction
    this.options =
      [
      ]
  }
  render () {
    let flat = {
      txHash: this.data.txHash.substring(0, 32) + '...',
      timestamp: moment.unix(this.data.timestamp).fromNow(),
      totalInput: this.data.totalInput.satoshi,
      totalOutput: this.data.totalOutput.satoshi
    }
    this.root.innerHTML = replace(this.template, {...this.data, ...flat})
    return this.root
  }
  requestData () {
    return {id: this.data.address, type: 'address', index: this.index}
  }
}
