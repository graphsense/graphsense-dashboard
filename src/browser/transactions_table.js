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
        data: 'tx_hash',
        render: this.formatValue(this.truncateValue)
      },
      {
        name: t('Value'),
        data: 'value',
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
    this.loadMessage = 'loadTransactions'
    this.resultField = 'txs'
    this.selectMessage = 'clickTransaction'
    this.loadParams = [this.nodeId, this.nodeType]
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
