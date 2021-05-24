import { t } from '../lang.js'
import TransactionsTable from './transactions_table.js'

export default class AccountTransactionsTable extends TransactionsTable {
  constructor (dispatcher, index, total, nodeId, nodeType, currency, keyspace) {
    super(dispatcher, index, total, nodeId, nodeType, currency, keyspace)
    this.loadMessage = 'loadTransactions'
    this.resultField = 'address_txs'
    this.selectMessage = 'clickTransaction'
    this.loadParams = [this.nodeId, this.nodeType]
    this.columns.forEach(col => {
      if (col.data === 'value') {
        col.data = 'values'
      }
    })
  }

  getParams () {
    return {
      id: this.loadParams[0],
      type: this.loadParams[1],
      keyspace: this.keyspace
    }
  }
}
