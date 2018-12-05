import Table from './table.js'

export default class NeighborsTable extends Table {
  constructor (dispatcher, index, total, id, type, isOutgoing, currency, keyspace) {
    super(dispatcher, index, total, currency, keyspace)
    this.isOutgoing = isOutgoing
    this.columns = [
      { name: (isOutgoing ? 'Outgoing ' : 'Incoming ') + type,
        data: 'id'
      },
      { name: 'Balance',
        data: 'balance',
        render: (value) => {
          return this.formatCurrency(value[this.currency], keyspace)
        }
      },
      { name: 'Received',
        data: 'received',
        render: (value) => {
          return this.formatCurrency(value[this.currency], keyspace)
        }
      },
      { name: 'No. Tx',
        data: 'noTransactions'
      }
    ]
    this.loadMessage = 'loadNeighbors'
    this.resultField = 'neighbors'
    this.selectMessage = 'selectNeighbor'
    this.loadParams = [id, type, isOutgoing]
  }
  isSmall () {
    return this.total < 2000
  }
}
