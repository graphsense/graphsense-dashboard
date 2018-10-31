import table from './transactions_table.html'
import Table from './table.js'

export default class TransactionsTable extends Table {
  constructor (dispatcher, total, nodeId, nodeType) {
    super(dispatcher, total)
    this.nodeId = nodeId
    this.nodeType = nodeType
    this.columns = [
      {data: 'txHash'},
      {data: 'value.satoshi'},
      {data: 'height'},
      {data: 'timestamp'}
    ]
    this.loadMessage = 'loadTransactions'
    this.resultMessage = 'resultTransactions'
    this.resultField = 'transactions'
    this.loadParams = [this.nodeId, this.nodeType]
  }
  isSmall () {
    return this.total < 200
  }
  render () {
    return super.render(table)
  }
}
