import { t } from '../lang.js'
import Table from './table.js'

export default class TransactionsTable extends Table {
  constructor (dispatcher, index, total, nodeId, nodeType, currency, keyspace) {
    super(dispatcher, index, total, currency, keyspace)
    this.nodeId = nodeId
    this.nodeType = nodeType
    this.columns = [
      {
        name: t('Transaction'),
        data: 'tx_hash'
      },
      {
        name: t('Value'),
        data: row => this.getValueByCurrencyCode(row.value),
        className: 'text-right',
        render: (value, type) =>
          this.formatCurrencyInTable(type, value, keyspace, true)
      },
      {
        name: t('Height'),
        data: 'height',
        className: 'text-right'
      },
      {
        name: t('Timestamp'),
        data: 'timestamp',
        render: this.formatValue(this.formatTimestamp)
      }
    ]
    this.loadMessage = 'loadTransactions'
    this.resultField = 'address_txs'
    this.selectMessage = 'clickTransaction'
    this.loadParams = [this.nodeId, this.nodeType]
    this.addOption(this.downloadOption(t('Transactions file', t(this.nodeType), this.nodeId) + ` (${keyspace.toUpperCase()})`))
  }

  getParams () {
    return {
      id: this.loadParams[0],
      type: this.loadParams[1],
      keyspace: this.keyspace
    }
  }
}
