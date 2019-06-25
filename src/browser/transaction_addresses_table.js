import Table from './table.js'

export default class TransactionAddressesTable extends Table {
  constructor (dispatcher, data, isOutgoing, index, currency, keyspace, nodeIsInGraph) {
    let addresses = isOutgoing ? data.outputs : data.inputs
    let label = isOutgoing ? 'Output addresses' : 'Input addresses'
    super(dispatcher, index, addresses.length, currency, keyspace)
    this.columns = [
      { name: label,
        data: 'address',
        render: this.formatIsInGraph(nodeIsInGraph, 'address', keyspace)
      },
      { name: 'Value',
        data: 'value',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      }
    ]
    this.data = addresses
    this.selectMessage = 'clickAddress'
  }
  isSmall () {
    return true
  }
  destroy () {
  }
}
