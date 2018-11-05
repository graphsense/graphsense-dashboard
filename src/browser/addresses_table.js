import Table from './table.js'

export default class AddressesTable extends Table {
  constructor (dispatcher, index, total, clusterId) {
    super(dispatcher, index, total)
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
      }
    ]
    this.loadMessage = 'loadAddresses'
    this.resultMessage = 'resultAddresses'
    this.resultField = 'addresses'
    this.selectMessage = 'selectAddress'
    this.loadParams = this.clusterId
  }
  isSmall () {
    return this.total < 200
  }
}
