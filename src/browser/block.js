import block from './block.html'
import moment from 'moment'
import {replace} from '../template_utils'
import BrowserComponent from './component.js'

export default class Block extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = block
    this.options =
      [
        {icon: 'exchange-alt', optionText: 'Transactions', message: 'initBlockTransactionsTable'}
      ]
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    let flat = {
      height: this.data.height,
      timestamp: this.formatTimestampWithAgo(this.data.timestamp),
      block_hash: this.data.block_hash,
      no_txs: this.data.no_txs,
      keyspace: this.data.keyspace.toUpperCase()
    }
    this.root.innerHTML = replace(this.template, {...this.data, ...flat})
    return this.root
  }
  requestData () {
    return {...super.requestData(), id: this.data.height, type: 'block'}
  }
}
