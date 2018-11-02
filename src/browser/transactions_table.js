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
        data: 'value.satoshi'
      },
      { name: 'Height',
        data: 'height'
      },
      { name: 'Timestamp',
        data: 'timestamp'
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
