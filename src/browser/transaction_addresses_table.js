import { t } from '../lang.js'
import Table from './table.js'
import { maxAddableNodes } from '../globals.js'

export default class TransactionAddressesTable extends Table {
  constructor (dispatcher, data, isOutgoing, index, currency, keyspace, nodeIsInGraph) {
    const addresses = isOutgoing ? data.outputs : data.inputs
    const label = isOutgoing ? t('Output address') : t('Input address')
    super(dispatcher, index, addresses.length, currency, keyspace)
    this.isOutgoing = isOutgoing
    this.columns = [
      {
        name: label,
        data: 'address',
        render: this.formatIsInGraph(nodeIsInGraph, 'address', keyspace)
      },
      {
        name: t('Value'),
        data: 'value',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      }
    ]
    this.data = addresses
    this.selectMessage = 'clickAddress'
    if (addresses.length < maxAddableNodes) this.addOption(this.addAllOption())
  }

  isSmall () {
    return true
  }

  destroy () {
  }
}
