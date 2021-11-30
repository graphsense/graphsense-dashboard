import { t } from '../lang.js'
import TransactionsTable from './transactions_table.js'

export default class AccountTransactionsTable extends TransactionsTable {
  constructor (dispatcher, index, total, nodeId, nodeType, currency, keyspace) {
    super(dispatcher, index, total, nodeId, nodeType, currency, keyspace)
    this.loadMessage = 'loadTransactions'
    this.selectMessage = 'clickTransaction'
    this.loadParams = [this.nodeId, this.nodeType]
    this.columns = this.columns.concat([
      {
        name: t('From address'),
        data: 'from_address'
      },
      {
        name: t('To address'),
        data: 'to_address'
      }
    ])
  }

  getParams () {
    return {
      id: this.loadParams[0],
      type: this.loadParams[1],
      keyspace: this.keyspace
    }
  }
}
