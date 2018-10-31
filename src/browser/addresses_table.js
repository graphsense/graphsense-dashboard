import Table from './table.js'

export default class AddressesTable extends Table {
  constructor (dispatcher, total, clusterId) {
    super(dispatcher, total)
    this.clusterId = clusterId
    this.columns = [
      { name: 'Address',
        data: 'address'
      },
      { name: 'First&nbsp;usage',
        data: 'firstTx.timestamp'
      },
      { name: 'Last&nbsp;usage',
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
