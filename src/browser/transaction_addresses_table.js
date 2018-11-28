import Table from './table.js'

export default class TransactionAddressesTable extends Table {
  constructor (dispatcher, addresses, label, index, currency) {
    super(dispatcher, index, addresses.length, currency)
    this.columns = [
      { name: label,
        data: 'address'
      },
      { name: 'Value',
        data: 'value',
        render: (value) => {
          return this.formatCurrency(value[this.currency])
        }
      }
    ]
    this.data = addresses
    this.selectMessage = 'clickAddress'
  }
  isSmall () {
    return true
  }
  destroy () {
  }
}
