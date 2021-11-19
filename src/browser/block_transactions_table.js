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
        data: 'inputs',
        render: value => value.length
      },
      {
        name: t('No. outputs'),
        data: 'outputs',
        render: value => value.length
      },
      {
        name: t('Total input'),
        data: row => row.total_input.value,
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
      },
      {
        name: t('Total output'),
        data: row => row.total_output.value,
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
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
