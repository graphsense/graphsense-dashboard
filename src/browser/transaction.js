import { tt } from '../lang.js'
import transaction from './transaction.html'
import { replace } from '../template_utils'
import BrowserComponent from './component.js'

export default class Transaction extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = transaction
    this.options =
      [
        { inline: 'row-incoming', optionText: 'Sending addresses', message: 'initTxInputsTable' },
        { inline: 'row-outgoing', optionText: 'Receiving addresses', message: 'initTxOutputsTable' }
      ]
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    const flat = {
      tx_hash: this.data.tx_hash,
      timestamp: this.formatTimestampWithAgo(this.data.timestamp),
      no_inputs: this.data.inputs.length,
      no_outputs: this.data.outputs.length,
      total_input: this.formatCurrency(this.data.total_input, this.data.keyspace),
      total_output: this.formatCurrency(this.data.total_output, this.data.keyspace)
    }
    this.root.innerHTML = replace(tt(this.template), { ...this.data, ...flat })
    this.renderInlineOptions()
    return this.root
  }

  requestData () {
    return { ...super.requestData(), id: this.data.address, type: 'address' }
  }
}
