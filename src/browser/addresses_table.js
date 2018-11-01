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
        data: 'firstTx.timestamp'
      },
      { name: 'Last usage',
        data: 'lastTx.timestamp'
      },
      { name: 'Balance',
        data: 'balance.satoshi'
      },
      { name: 'Received',
        data: 'totalReceived.satoshi'
      }
    ]
    this.loadMessage = 'loadAddresses'
    this.resultMessage = 'resultAddresses'
    this.resultField = 'addresses'
    this.loadParams = this.clusterId
  }
  isSmall () {
    return this.total < 200
  }
}
