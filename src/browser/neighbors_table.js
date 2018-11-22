import Table from './table.js'

export default class NeighborsTable extends Table {
  constructor (dispatcher, index, total, id, type, isOutgoing) {
    super(dispatcher, index, total)
    this.isOutgoing = isOutgoing
    this.columns = [
      { name: (isOutgoing ? 'Outgoing ' : 'Incoming ') + type,
        data: 'id'
      },
      { name: 'Balance',
        data: 'balance.satoshi',
        render: (value) => {
          return this.formatCurrency(value)
        }
      },
      { name: 'Received',
        data: 'totalReceived.satoshi',
        render: (value) => {
          return this.formatCurrency(value)
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
    return this.total < 200
  }
}
