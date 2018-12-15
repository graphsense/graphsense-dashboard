import Table from './table.js'

export default class TransactionsTable extends Table {
  constructor (dispatcher, index, total, nodeId, nodeType, currency, keyspace) {
    super(dispatcher, index, total, currency, keyspace)
    this.nodeId = nodeId
    this.nodeType = nodeType
    this.columns = [
      { name: 'Transaction',
        data: 'txHash',
        render: (value) => {
          return this.truncateValue(value)
        }
      },
      { name: 'Value',
        data: 'value',
        className: 'text-right',
        render: (value) => {
          return this.formatCurrency(value[this.currency], keyspace, true)
        }
      },
      { name: 'Height',
        data: 'height'
      },
      { name: 'Timestamp',
        data: 'timestamp',
        render: this.formatTimestamp
      }
    ]
    this.loadMessage = 'loadTransactions'
    this.resultField = 'transactions'
    this.selectMessage = 'clickTransaction'
    this.loadParams = [this.nodeId, this.nodeType]
  }
  isSmall () {
    return this.total < 200
  }
}
