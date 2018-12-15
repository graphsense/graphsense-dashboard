import Table from './table.js'

export default class TransactionAddressesTable extends Table {
  constructor (dispatcher, addresses, label, index, currency, keyspace) {
    super(dispatcher, index, addresses.length, currency, keyspace)
    this.columns = [
      { name: label,
        data: 'address'
      },
      { name: 'Value',
        data: 'value',
        className: 'text-right',
        render: (value) => {
          return this.formatCurrency(value[this.currency], keyspace)
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
