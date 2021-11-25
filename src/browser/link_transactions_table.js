import { t } from '../lang.js'
import Table from './table.js'

export default class LinkTransactionsTable extends Table {
  constructor (dispatcher, index, source, target, total, nodeType, currency, keyspace) {
    console.log('total', total)
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
        data: row => this.getValueByCurrencyCode(row.input_value),
        className: 'text-right',
        render: (value, type) => this.formatCurrency(value, keyspace, true)
      },
      {
        name: t('Output value'),
        data: row => this.getValueByCurrencyCode(row.output_value),
        className: 'text-right',
        render: (value, type) => this.formatCurrency(value, keyspace, true)
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
        data: row => this.getValueByCurrencyCode(row.value),
        className: 'text-right',
        render: (value, type) =>
          this.formatCurrency(value, keyspace, true)
      }
      this.columns.splice(1, 2, col)
    }
    this.loadMessage = 'loadLinkTransactions'
    this.resultField = 'links'
    this.selectMessage = 'clickTransaction'
    this.loadParams = { source: this.source, target: this.target, type: this.nodeType }
    this.addOption(this.downloadOption(t('links file', t(this.nodeType), this.source, this.target) + ` (${keyspace.toUpperCase()})`))
  }

  getParams () {
    return {
      ...this.loadParams,
      keyspace: this.keyspace
    }
  }
}
