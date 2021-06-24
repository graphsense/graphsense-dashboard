import { t } from '../lang.js'
import Table from './table.js'

export default class BlockTransactionsTable extends Table {
  constructor (dispatcher, index, total, height, currency, keyspace) {
    super(dispatcher, index, total, currency, keyspace)
    this.height = height
    this.columns = [
      {
        name: t('Transaction'),
        data: 'tx_hash',
        render: this.formatValue(this.truncateValue)
      },
      {
        name: t('No. inputs'),
        data: 'no_inputs'
      },
      {
        name: t('No. outputs'),
        data: 'no_outputs'
      },
      {
        name: t('Total input'),
        data: 'total_input',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      },
      {
        name: t('Total output'),
        data: 'total_output',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      }
    ]
    this.loadMessage = 'loadTransactions'
    this.resultField = null
    this.selectMessage = 'clickTransaction'
    this.loadParams = [this.height, 'block']
    this.addOption(this.downloadOption())
  }

  getParams () {
    return {
      id: this.loadParams[0],
      type: this.loadParams[1],
      keyspace: this.keyspace
    }
  }
}
