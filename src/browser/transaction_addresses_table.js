import Table from './table.js'

export default class TransactionAddressesTable extends Table {
  constructor (dispatcher, addresses, label, index) {
    super(dispatcher, index, addresses.length)
    this.columns = [
      { name: label,
        data: 'address'
      },
      { name: 'Value',
        data: 'value.satoshi',
        render: (value) => {
          return this.formatCurrency(value)
        }
      }
    ]
    this.data = addresses
    this.selectMessage = 'loadAddress'
  }
  isSmall () {
    return true
  }
  destroy () {
  }
}
