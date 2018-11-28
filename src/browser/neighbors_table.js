import Table from './table.js'

export default class NeighborsTable extends Table {
  constructor (dispatcher, index, total, id, type, isOutgoing, currency) {
    super(dispatcher, index, total, currency)
    this.isOutgoing = isOutgoing
    this.columns = [
      { name: (isOutgoing ? 'Outgoing ' : 'Incoming ') + type,
        data: 'id'
      },
      { name: 'Balance',
        data: 'balance',
        render: (value) => {
          return this.formatCurrency(value[this.currency])
        }
      },
      { name: 'Received',
        data: 'received',
        render: (value) => {
          console.log('value', value)
          return this.formatCurrency(value[this.currency])
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
