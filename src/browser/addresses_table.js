import { t } from '../lang.js'
import Table from './table.js'
import { maxAddableNodes } from '../globals.js'

export default class AddressesTable extends Table {
  constructor (dispatcher, index, total, entityId, currency, keyspace, nodeIsInGraph) {
    super(dispatcher, index, total, currency, keyspace)
    this.entityId = entityId
    this.columns = [
      {
        name: t('Address'),
        data: 'address',
        render: this.formatIsInGraph(nodeIsInGraph, 'address', keyspace)
      },
      {
        name: t('First usage'),
        data: 'first_tx.timestamp',
        render: this.formatValue(this.formatTimestamp)
      },
      {
        name: t('Last usage'),
        data: 'last_tx.timestamp',
        render: this.formatValue(this.formatTimestamp)
      },
      {
        name: t('Final balance'),
        data: row => row.balance.value,
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
      },
      {
        name: t('Total received'),
        data: row => row.total_received.value,
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
      }
    ]
    this.loadMessage = 'loadAddresses'
    this.resultField = 'addresses'
    this.selectMessage = 'selectAddress'
    this.loadParams = this.entityId
    if (total < maxAddableNodes) this.addOption(this.addAllOption())
    this.addOption(this.downloadOption(t('Addresses file', entityId) + ` (${keyspace.toUpperCase()})`))
  }

  getParams () {
    return {
      id: this.entityId,
      keyspace: this.keyspace
    }
  }
}
