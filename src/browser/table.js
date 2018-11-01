import 'jquery'
import 'datatables.net'
import 'datatables.net-scroller'
import {browserHeight, browserPadding} from '../globals.js'
import table from './table.html'
import BrowserComponent from './component.js'

const rowHeight = 30

export default class Table extends BrowserComponent {
  constructor (dispatcher, index, total) {
    super(dispatcher, index)
    this.nextPage = null
    this.total = total
    this.data = []
    this.loading = null
    this._id = Math.random()
  }
  resultEvent () {
    return this.resultMessage + '.' + this._id
  }
  isSmall () {
    return this.total < 5000
  }
  render () {
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
    // DataTable Scroller needs DataTable to be present in the DOM
    // so wait a ms for it to be inserted upstream ... hackish!
    setTimeout(() => {
      let table = $(this.root).children().first().DataTable({
        ajax: (request, drawCallback, settings) => {
          this.ajax(request, drawCallback, settings, this)
        },
        scrollY: browserHeight - rowHeight - 4 * browserPadding,
        searching: false,
        ordering: this.isSmall(),
        deferRender: true,
        scroller: {
          rowHeight: 'auto',
          serverWait: 50,
          loadingIndicator: true
        },
        stateSave: false,
        serverSide: !this.isSmall(),

        columns: this.columns
      })
      // using es5 'function' to have 'this' bound to the triggering element
      $(this.root).on('click', 'tr', function () {
        let row = table.row(this).data()
        that.dispatcher.call(that.selectMessage, null, row)
      })
    }, 1)
    return this.root
  }
  renderOptions () {
    return null
  }
  ajax (request, drawCallback, settings, table) {
    console.log('ajax request', request)
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
    if (table.loading) {
      if (request.start + request.length <= table.data.length + table.loading.length) {
        // update request while loading
        table.loading = request
      }
      return
    }

    table.dispatcher.on(table.resultEvent(), (response) => {
      if (!table.isSmall() && response.page !== table.nextPage) return
      table.data = table.data.concat(response.result[table.resultField])
      table.nextPage = response.result.nextPage
      data.data = table.data.slice(table.loading.start, table.loading.start + table.loading.length)
      data.draw = table.loading.draw
      table.loading = null
      drawCallback(data)
      table.dispatcher.on(table.resultEvent(), null)
    })
    let r =
      {
        params: table.loadParams,
        nextPage: table.nextPage,
        pagesize: request.length
      }
    table.dispatcher.call(table.loadMessage, null, r)
    table.loading = request
  }
  destroy () {
    this.dispatcher.on(this.resultEvent(), null)
  }
}
