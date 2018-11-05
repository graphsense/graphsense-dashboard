import Table from './table.js'

export default class TransactionsTable extends Table {
  constructor (dispatcher, index, total, nodeId, nodeType) {
    super(dispatcher, index, total)
    this.nodeId = nodeId
    this.nodeType = nodeType
    this.columns = [
      { name: 'Transaction',
        data: 'txHash'
      },
      { name: 'Value',
        data: 'value.satoshi',
        render: (value) => {
          return this.formatCurrency(value)
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
    this.resultMessage = 'resultTransactions'
    this.resultField = 'transactions'
    this.selectMessage = 'loadTransaction'
    this.loadParams = [this.nodeId, this.nodeType]
  }
  isSmall () {
    return this.total < 200
  }
}
