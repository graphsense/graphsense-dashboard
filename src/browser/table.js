import $ from 'jquery'
import 'datatables.net'
import 'datatables.net-scroller'
import { browserHeight, browserPadding } from '../globals.js'
import table from './table.html'
import BrowserComponent from './component.js'
import Logger from '../logger.js'
import numeral from 'numeral'
import downloadCSV from '../icons/downloadCSV.html'

const logger = Logger.create('BrowserTable') // eslint-disable-line no-unused-vars

const rowHeight = 24

export default class Table extends BrowserComponent {
  constructor (dispatcher, index, total, currency, keyspace) {
    super(dispatcher, index, currency)
    this.keyspace = keyspace
    this.nextPage = null
    this.total = total
    this.data = []
    this.loading = null
    this.searchable = false
    if (this.isSmall()) {
      this.addOption({ icon: 'search', optionText: 'Filter table contents', message: 'toggleSearchTable' })
    }
  }

  smallThreshold () {
    return 10000
  }

  isSmall () {
    return this.total < this.smallThreshold()
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    logger.debug('shouldupdate', this.update)
    if (!this.shouldUpdate()) return this.root
    if (this.shouldUpdate(true)) {
      logger.debug('render table')
      super.render()
      this.root.innerHTML = table
      const tr = this.root.querySelector('tr')
      const el = this.root.querySelector('th')
      this.columns.forEach((column, i) => {
        const el2 = el.cloneNode()
        el2.innerHTML = column.name.replace(/ /g, '&nbsp;')
        tr.appendChild(el2)
      })
      tr.removeChild(el)
      const that = this
      const total = numeral(this.total).format('1,000')
      const tab = this.table = $(this.root).children().first().DataTable({
        ajax: (request, drawCallback, settings) => {
          this.ajax(request, drawCallback, settings, this)
        },
        scrollY: browserHeight - rowHeight - 4 * browserPadding,
        searching: this.searchable && this.isSmall(),
        search: { smart: false },
        dom: 'fti',
        ordering: this.isSmall(),
        order: this.order,
        deferRender: true,
        scroller: {
          loadingIndicator: true,
          displayBuffer: 20,
          boundaryScale: 0
        },
        stateSave: false,
        serverSide: !this.isSmall(),
        columns: this.columns,
        language: {
          info: `Showing _START_ to _END_ of ${this.isSmall() ? '_TOTAL_' : total} entries` + (!this.isSmall() ? ` <span class="text-gs-red">(>${numeral(this.smallThreshold()).format('1,000')} - sort/filter disabled)</span>` : '')
        }
      })
      // using es5 'function' to have 'this' bound to the triggering element
      this.table.on('click', 'td', function (e) {
        if (!that.selectMessage) return
        const cell = tab.cell(this)
        if (!cell) return
        const index = cell.index()
        logger.debug('index', index)
        const row = tab.row(index.row).data()
        logger.debug('row', row)
        if (!row.keyspace) {
          row.keyspace = that.keyspace
        }
        let msgs = that.selectMessage
        if (!Array.isArray(msgs)) {
          msgs = [msgs]
        }
        if (!msgs[index.column]) return
        that.dispatcher(msgs[index.column], row)
      })
      this.table.on('order.dt', () => {
        this.order = this.table.order()
      })
      return this.root
    }
    if (this.shouldUpdate('page')) {
      logger.debug('redraw table')
      this.table.rows().invalidate('data').draw('page')
      super.render()
    }
  }

  toggleSearch () {
    this.searchable = !this.searchable
    this.setUpdate(true)
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
      const total = Math.min(table.total, table.data.length + 1)
      const data = {
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
    const r =
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

  setResponse ({ page, request, drawCallback, result }) {
    if (!this.isSmall() && page !== this.nextPage) return
    this.data = this.data.concat(this.resultField ? result[this.resultField] : result)
    logger.debug('data', result, this.resultField, this.data)
    this.nextPage = result.next_page
    const loading = this.loading || request
    // HACK: The table shall only be scrollable to the currently loaded data.
    // Add +1 so DataTables triggers loading more data when scrolling to the end.
    const total = Math.min(this.total, this.data.length + 1)
    const data = {
      draw: request.draw,
      recordsTotal: total,
      recordsFiltered: total,
      data: this.data.slice(loading.start, loading.start + loading.length)
    }
    this.loading = null
    drawCallback(data)
  }

  truncateValue (value) {
    return value ? `<span title="${value}">${value.substr(0, 20)}${value.length > 20 ? '...' : ''}</span>` : ''
  }

  formatLink (value) {
    if (!value) return ''
    if (value.startsWith('http')) {
      return `<a onClick="event.stopPropagation()" href="${value}" target=_blank>${this.truncateValue(value)}</a>`
    }
    return value
  }

  formatValue (func) {
    return (value, type) => {
      if (type === 'display') return func(value)
      return value
    }
  }

  downloadOption () {
    return { html: downloadCSV, optionText: 'Download table as CSV', message: 'downloadTable' }
  }

  addAllOption () {
    return { icon: 'plus-square', optionText: 'Add all to graph', message: 'addAllToGraph' }
  }

  formatIsInGraph (nodeIsInGraph, type, keyspace) {
    return (value, t) => {
      if (t === 'display') {
        if (nodeIsInGraph(value, type, keyspace)) {
          return '<i class="fas fa-check text-xs mr-1"></i>' + value
        }
        return value
      }
      return value
    }
  }
}
