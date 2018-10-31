import table from './addresses_table.html'
import Table from './table.js'

export default class AddressesTable extends Table {
  constructor (dispatcher, total, clusterId) {
    super(dispatcher, total)
    this.clusterId = clusterId
    this.columns = [
      {data: 'address'},
      {data: 'firstTx.timestamp'},
      {data: 'lastTx.timestamp'},
      {data: 'balance.satoshi'},
      {data: 'totalReceived.satoshi'}
    ]
    this.loadMessage = 'loadAddresses'
    this.resultMessage = 'resultAddresses'
    this.resultField = 'addresses'
    this.loadParams = this.clusterId
  }
  isSmall () {
    return this.total < 200
  }
  render () {
    return super.render(table)
  }
}
