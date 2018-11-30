import Table from './table.js'

export default class AddressesTable extends Table {
  constructor (dispatcher, index, total, clusterId, currency, keyspace) {
    super(dispatcher, index, total, currency, keyspace)
    this.clusterId = clusterId
    this.columns = [
      { name: 'Address',
        data: 'address'
      },
      { name: 'First usage',
        data: 'firstTx.timestamp',
        render: this.formatTimestamp
      },
      { name: 'Last usage',
        data: 'lastTx.timestamp',
        render: this.formatTimestamp
      },
      { name: 'Balance',
        data: 'balance',
        render: (value) => {
          return this.formatCurrency(value[this.currency], keyspace)
        }
      },
      { name: 'Received',
        data: 'totalReceived',
        render: (value) => {
          return this.formatCurrency(value[this.currency], keyspace)
        }
      }
    ]
    this.loadMessage = 'loadAddresses'
    this.resultField = 'addresses'
    this.selectMessage = 'selectAddress'
    this.loadParams = this.clusterId
  }
  isSmall () {
    return this.total < 200
  }
}
