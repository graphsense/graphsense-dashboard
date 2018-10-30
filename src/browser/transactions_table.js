import table from './transactions_table.html'
import * as $ from 'jquery'
import * as dt from 'datatables.net'

export default class TransactionsTable {
  constructor (dispatcher, nodeId, nodeType) {
    this.dispatcher = dispatcher
    this.nodeId = nodeId
    this.nodeType = nodeType
  }
  render () {
    this.root = document.createElement('div')
    this.root.innerHTML = table
    $(this.root).children().first().DataTable({
      ajax: (request, drawCallback, settings) => {
        this.ajax(
          {id: this.nodeId, type: this.nodeType, ...request},
          drawCallback,
          {dispatcher: this.dispatcher, ...settings}
        )
      },
      columns: [
        {data: 'txHash'},
        {data: 'value.satoshi'},
        {data: 'height'},
        {data: 'timestamp'}
      ]
    })
    return this.root
  }
  renderOptions () {
    return null
  }
  ajax (request, drawCallback, settings) {
    console.log('ajax request', request)
    settings.dispatcher.call('loadTransactions', null, request)
    settings.dispatcher.on('resultTransactions.transactions_table', (response) => {
      console.log('resultTransactions table', response)
      let data = {data: response.result}
      drawCallback(data)
      this.dispatcher.on('resultTransactions.transactions_table', null)
    })
  }
}
