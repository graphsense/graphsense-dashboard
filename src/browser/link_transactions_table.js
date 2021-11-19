import { t } from '../lang.js'
import Table from './table.js'

export default class LinkTransactionsTable extends Table {
  constructor (dispatcher, index, source, target, total, nodeType, currency, keyspace) {
    super(dispatcher, index, total, currency, keyspace)
    this.source = source
    this.target = target
    this.nodeType = nodeType
    this.columns = [
      {
        name: t('Transaction'),
        data: 'tx_hash',
        render: this.formatValue(this.truncateValue)
      },
      {
        name: t('Input value'),
        data: row => row.input_value.value,
        className: 'text-right',
        render: (value, type) => {
          return this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
        }
      },
      {
        name: t('Output value'),
        data: row => row.output_value.value,
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
      },
      {
        name: t('Height'),
        data: 'height'
      },
      {
        name: t('Timestamp'),
        data: 'timestamp',
        render: this.formatValue(this.formatTimestamp)
      }
    ]
    if (keyspace === 'eth') {
      const col = {
        name: t('Value'),
        data: row => row.value.value,
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
      }
      this.columns.splice(1, 2, col)
    }
    this.loadMessage = 'loadLinkTransactions'
    this.resultField = 'links'
    this.selectMessage = 'clickTransaction'
    this.loadParams = { source: this.source, target: this.target, type: this.nodeType }
    this.addOption(this.downloadOption())
  }

  isSmall () {
    return true
  }

  getParams () {
    return {
      ...this.loadParams,
      keyspace: this.keyspace
    }
  }
}
