import { t, dtLanguagePack } from '../lang.js'
import { nbsp } from '../utils.js'
import $ from 'jquery'
import 'datatables.net'
import 'datatables.net-scroller'
import '../../node_modules/datatables.net-buttons/js/buttons.html5'
import 'datatables.net-buttons'
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
    this.realTotal = total
    this.data = []
    this.total = this.smallThreshold()
    if (this.isSmall()) this.total = total
    this.dom = 'Bfti'
    this.loading = null
    this.searchable = false
    if (this.isSmall()) {
      this.addOption({
        icon: 'search',
        optionText: t('Filter table contents'),
        message: 'toggleSearchTable'
      })
    }
  }

  smallThreshold () {
    return 5000
  }

  isSmall () {
    return this.realTotal < this.smallThreshold()
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
        el2.innerHTML = nbsp(column.name)
        tr.appendChild(el2)
      })
      tr.removeChild(el)
      const that = this
      const language = { ...dtLanguagePack }
      logger.debug('language', language)
      const info = language.info ? 'info' : 'sInfo'
      language[info] = language[info] + (!this.isSmall() ? ` <span class="text-gs-red">(>${numeral(this.smallThreshold()).format('1,000')} - ${t('sort/filter disabled')})</span>` : '')
      language[info] = language[info].replace('_TOTAL_', numeral(this.realTotal).format('1,000'))
      const tab = this.table = $(this.root).children().first().DataTable({
        ajax: (request, drawCallback, settings) => {
          this.ajax(request, drawCallback, settings, this)
        },
        scrollY: Math.max(this.root.getBoundingClientRect().height, browserHeight) - 1.5 * rowHeight,
        searching: this.searchable && this.isSmall(),
        search: { smart: false },
        dom: this.dom,
        ordering: this.isSmall(),
        order: this.order,
        deferRender: true,
        scroller: {
          loadingIndicator: true,
          displayBuffer: 20,
          boundaryScale: 1
        },
        stateSave: false,
        serverSide: this.serverSide(),
        columns: this.columns,
        buttons: [{
          extend: 'csv',
          filename: this.filename
        }],
        language
      })
      // using es5 'function' to have 'this' bound to the triggering element
      this.table.on('click', 'td', function (e) {
        if (!that.selectMessage) return
        const cell = tab.cell(this)
        if (!cell) return
        const index = cell.index()
        const row = tab.row(index.row).data()
        if (!that.isActiveRow(row)) return
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
    logger.debug('table.loading', table.loading)
    if (!table.serverSide()) {
      request.start = 0
      request.length = table.total
    }
    if (request.start + request.length <= table.data.length) {
      const data = {
        draw: request.draw,
        recordsTotal: this.total,
        recordsFiltered: this.total
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
    logger.debug('table.data', table.data.length)
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
    if (this.serverSide() && (page || null) !== this.nextPage) return
    this.data = this.data.concat(this.resultField ? result[this.resultField] : result)
    this.nextPage = result.next_page
    const loading = this.loading || request
    this.total = this.data.length
    if (!this.isSmall()) {
      if (this.realTotal === -1) {
        this.total += 100
      } else {
        this.total = Math.min(this.total + 100, this.realTotal)
      }
    }
    const data = {
      draw: request.draw,
      recordsTotal: this.total,
      recordsFiltered: this.total,
      data: this.data.slice(loading.start, loading.start + loading.length)
    }
    this.loading = null
    drawCallback(data)
    this.table.columns.adjust()
  }

  truncateValue (value) {
    return value ? `<span title="${value}">${value.substr(0, 20)}${value.length > 20 ? '...' : ''}</span>` : ''
  }

  formatLink (url, title, description) {
    if (url && url.startsWith('http')) {
      return `<a onClick="event.stopPropagation()" title="${description}" href="${url}" target="_blank">${title || this.truncateValue(url)}</a>`
    } else if (title) {
      return `<span title="${description}">${title}</span>`
    }
    return url || ''
  }

  formatValue (func, unformatted = null) {
    return (value, type) => {
      if (type === 'display') return func(value)
      return unformatted || value
    }
  }

  formatCurrencyInTable (type, value, keyspace, colorful) {
    if (type !== 'display') return value
    return this.formatCurrency(value, keyspace, colorful)
  }

  downloadOption (filename) {
    this.filename = filename
    return { html: downloadCSV, optionText: t('Download table as CSV'), message: 'downloadTable' }
  }

  addAllOption () {
    return { icon: 'plus-square', optionText: t('Add all to graph'), message: 'addAllToGraph' }
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

  isActiveRow () {
    return true
  }

  serverSide () {
    return !this.isSmall()
  }
}
