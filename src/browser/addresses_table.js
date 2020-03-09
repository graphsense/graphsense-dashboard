import Table from './table.js'
import { maxAddableNodes } from '../globals.js'

export default class AddressesTable extends Table {
  constructor (dispatcher, index, total, entityId, currency, keyspace, nodeIsInGraph) {
    super(dispatcher, index, total, currency, keyspace)
    this.entityId = entityId
    this.columns = [
      {
        name: 'Address',
        data: 'address',
        render: this.formatIsInGraph(nodeIsInGraph, 'address', keyspace)
      },
      {
        name: 'First usage',
        data: 'first_tx.timestamp',
        render: this.formatValue(this.formatTimestamp)
      },
      {
        name: 'Last usage',
        data: 'last_tx.timestamp',
        render: this.formatValue(this.formatTimestamp)
      },
      {
        name: 'Balance',
        data: 'balance',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      },
      {
        name: 'Received',
        data: 'total_received',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      }
    ]
    this.loadMessage = 'loadAddresses'
    this.resultField = 'addresses'
    this.selectMessage = 'selectAddress'
    this.loadParams = this.entityId
    if (total < maxAddableNodes) this.addOption(this.addAllOption())
  }
}
