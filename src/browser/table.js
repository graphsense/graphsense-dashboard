import 'jquery'
import 'datatables.net'
import 'datatables.net-scroller'
import {browserHeight, browserPadding} from '../globals.js'
import table from './table.html'
import BrowserComponent from './component.js'
import Logger from '../logger.js'

const logger = Logger.create('BrowserTable') // eslint-disable-line

const rowHeight = 24

export default class Table extends BrowserComponent {
  constructor (dispatcher, index, total, currency, keyspace) {
    super(dispatcher, index, currency)
    this.keyspace = keyspace
    this.nextPage = null
    this.total = total
    this.data = []
    this.loading = null
  }
  isSmall () {
    return this.total < 5000
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    logger.debug('render table')
    super.render()
    this.root.innerHTML = table
    let tr = this.root.querySelector('tr')
    let el = this.root.querySelector('th')
    this.columns.forEach(({name}) => {
      let el2 = el.cloneNode()
      el2.innerHTML = name.replace(/ /g, '&nbsp;')
      tr.appendChild(el2)
    })
    tr.removeChild(el)
    let that = this
    let tab = this.table = $(this.root).children().first().DataTable({
      ajax: (request, drawCallback, settings) => {
        this.ajax(request, drawCallback, settings, this)
      },
      scrollY: browserHeight - rowHeight - 4 * browserPadding,
      searching: false,
      ordering: this.isSmall(),
      deferRender: true,
      scroller: {
        loadingIndicator: true,
        displayBuffer: 100,
        boundaryScale: 0
      },
      stateSave: false,
      serverSide: !this.isSmall(),
      columns: this.columns,
      language: {
        info: `Showing _START_ to _END_ of ${this.total} entries`
      }
    })
    // using es5 'function' to have 'this' bound to the triggering element
    $(this.root).on('click', 'tr', function () {
      let row = tab.row(this).data()
      if (!row) return
      logger.debug('row', row)
      if (!row.keyspace) {
        row.keyspace = that.keyspace
      }
      that.dispatcher(that.selectMessage, row)
    })
    return this.root
  }
  renderOptions () {
    return null
  }
  ajax (request, drawCallback, settings, table) {
    logger.debug('ajax request', request)
    if (table.isSmall()) {
      request.start = 0
      request.length = table.total
    }
    if (request.start + request.length <= table.data.length) {
      // HACK: The table shall only be scrollable to the currently loaded data.
      // Add +1 so DataTables triggers loading more data when scrolling to the end.
      let total = Math.min(table.total, table.data.length + 1)
      let data = {
        draw: request.draw,
        recordsTotal: total,
        recordsFiltered: total
      }
      // data from cache
      data.data = table.data.slice(request.start, request.start + request.length)
      drawCallback(data)
      return
    }
    if (table.loading) {
      if (request.start + request.length <= table.data.length + table.loading.length) {
        // update request while loading
        table.loading = request
      }
      return
    }
    let r =
      {
        keyspace: table.keyspace,
        params: table.loadParams,
        nextPage: table.nextPage,
        request: request,
        drawCallback: drawCallback
      }
    table.dispatcher(table.loadMessage, r)
    table.loading = request
  }
  setResponse ({page, request, drawCallback, result}) {
    if (!this.isSmall() && page !== this.nextPage) return
    this.data = this.data.concat(result[this.resultField])
    this.nextPage = result.nextPage
    let loading = this.loading || request
    // HACK: The table shall only be scrollable to the currently loaded data.
    // Add +1 so DataTables triggers loading more data when scrolling to the end.
    let total = Math.min(this.total, this.data.length + 1)
    let data = {
      draw: request.draw,
      recordsTotal: total,
      recordsFiltered: total,
      data: this.data.slice(loading.start, loading.start + loading.length)
    }
    this.loading = null
    drawCallback(data)
  }
  truncateValue (value) {
    return value ? `<span title="${value}">${value.substr(0, 20)}...</span>` : ''
  }
  formatValue (func) {
    return (value, type) => {
      if (type === 'display') return func(value)
      return value
    }
  }
}
