import table from './transactions_table.html'
import 'jquery'
import 'datatables.net'
import 'datatables.net-scroller'
import {browserHeight, browserPadding} from '../globals.js'

const rowHeight = 30

export default class TransactionsTable {
  constructor (dispatcher, nodeId, nodeType, total) {
    this.dispatcher = dispatcher
    this.nodeId = nodeId
    this.nodeType = nodeType
    this.nextPage = null
    this.total = total
    this.data = []
  }
  isSmall () {
    return this.total < 200
  }
  render () {
    this.root = document.createElement('div')
    this.root.className = 'browser-component'
    this.root.innerHTML = table
    // DataTable Scroller needs DataTable to be present in the DOM
    // so wait a ms for it to be inserted upstream ... hackish!
    setTimeout(() => {
      $(this.root).children().first().DataTable({
        ajax: (request, drawCallback, settings) => {
          this.ajax(request, drawCallback, settings, this)
        },
        scrollY: browserHeight - rowHeight - 2 * browserPadding,
        searching: false,
        ordering: this.isSmall(),
        deferRender: true,
        scroller: {
          rowHeight: rowHeight
        },
        stateSave: false,
        serverSide: !this.isSmall(),

        columns: [
          {data: 'txHash'},
          {data: 'value.satoshi'},
          {data: 'height'},
          {data: 'timestamp'}
        ]
      })
    }, 1)
    return this.root
  }
  renderOptions () {
    return null
  }
  ajax (request, drawCallback, settings, table) {
    if (table.isSmall()) {
      request.start = 0
      request.length = table.total
    }
    let data = {
      draw: request.draw,
      recordsTotal: table.total,
      recordsFiltered: table.total
    }
    if (request.start + request.length <= table.data.length) {
      // data from cache
      data.data = table.data.slice(request.start, request.start + request.length)
      drawCallback(data)
      return
    }
    if (table.loading) return

    table.dispatcher.on('resultTransactions.transactions_table', (response) => {
      if (response.page !== table.nextPage) return
      table.loading = false
      table.data = table.data.concat(response.result.transactions)
      table.nextPage = response.result.nextPage
      data.data = table.data.slice(request.start, request.start + request.length)
      drawCallback(data)
      table.dispatcher.on('resultTransactions.transactions_table', null)
    })
    let r =
      {
        id: table.nodeId,
        type: table.nodeType,
        nextPage: table.nextPage,
        pagesize: request.length
      }
    table.dispatcher.call('loadTransactions', null, r)
    table.loading = true
  }
}
