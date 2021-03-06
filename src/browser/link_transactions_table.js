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
        data: 'input_value',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      },
      {
        name: t('Output value'),
        data: 'output_value',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
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
        data: 'values',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      }
      this.columns.splice(1, 2, col)
    }
    this.loadMessage = 'loadLinkTransactions'
    this.resultField = null
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
