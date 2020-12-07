import { tt } from './lang.js'
import Logger from './logger.js'
import layout from './layout/layout.html'
import Component from './component.js'
import currency from './layout/currency.html'
import { addClass, removeClass } from './template_utils.js'
import { select } from 'd3-selection'

const logger = Logger.create('Layout') // eslint-disable-line no-unused-vars

export default class Layout extends Component {
  constructor (dispatcher, browser, graph, config, menu, search, status, currency) {
    super()
    this.currency = currency
    this.dispatcher = dispatcher
    this.browser = browser
    this.graph = graph
    this.config = config
    this.menu = menu
    this.search = search
    this.statusbar = status
    this.currencyRoot = null
    this.disabled = {}
  }

  triggerFileLoad (loadType) {
    this.root.querySelector(`.file-loader[data-type="${loadType}"]`).click()
  }

  triggerDownloadViaLink (url) {
    const a = this.root.querySelector('a#downloadCSV')
    a.setAttribute('href', url)
    a.click()
  }

  setCurrency (currency) {
    this.currency = currency
    this.setUpdate('currency')
  }

  disableButton (name, disable) {
    this.disabled[name] = disable
    this.setUpdate('buttons')
  }

  showModal (modal) {
    this.modal = modal
    this.setUpdate('modal')
  }

  hideModal () {
    this.modal = null
    this.setUpdate('modal')
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    // set overflow hidden to prevent window scrollbars
    document.body.style.overflow = 'hidden hidden'
    let browserRoot = null
    let graphRoot = null
    let configRoot = null
    let menuRoot = null
    let searchRoot = null
    let statusRoot = null
    let modalRoot = null
    if (this.shouldUpdate(true)) {
      this.root.innerHTML = tt(layout)
      this.browser.setUpdate(true)
      this.graph.setUpdate(true)
      this.config.setUpdate(true)
      this.menu.setUpdate(true)
      this.search.setUpdate(true)
      this.statusbar.setUpdate(true)
      this.renderButtons()
      const loaders = this.root.querySelectorAll('.file-loader')
      loaders.forEach(loader => {
        loader.addEventListener('change', (e) => {
          const input = e.target
          const type = e.target.getAttribute('data-type')
          const accept = e.target.getAttribute('accept')

          const reader = new FileReader() // eslint-disable-line no-undef
          const filename = input.files[0].name
          reader.onload = () => {
            const data = reader.result
            this.dispatcher('loadFile', [type, data, filename])
          }
          if (accept === '.gs') {
            reader.readAsArrayBuffer(input.files[0])
          } else {
            reader.readAsText(input.files[0])
          }
        })
      })
      this.root.querySelector('#layout-logo').addEventListener('click', () => {
        this.dispatcher('gohome')
      })
      browserRoot = this.root.querySelector('#layout-browser')
      graphRoot = this.root.querySelector('#layout-graph')
      configRoot = this.root.querySelector('#layout-config')
      menuRoot = this.root.querySelector('#layout-menu')
      searchRoot = this.root.querySelector('#layout-search')
      statusRoot = this.root.querySelector('#layout-status')
      this.currencyRoot = this.root.querySelector('#layout-currency-config')
    } else if (this.shouldUpdate('buttons')) {
      this.renderButtons()
    } else if (this.shouldUpdate('modal')) {
      modalRoot = this.root.querySelector('#modal > div')
      const style = this.root.querySelector('#modal').style
      if (this.modal) {
        style.display = 'flex'
        this.modal.render(modalRoot)
      } else {
        style.display = 'none'
      }
    }
    this.browser.render(browserRoot)
    this.graph.render(graphRoot)
    this.config.render(configRoot)
    this.menu.render(menuRoot)
    this.search.render(searchRoot)
    this.statusbar.render(statusRoot)
    this.renderCurrency()
    super.render()
    return this.root
  }

  renderButtons () {
    const navbarButtons =
        [['blank', 'blank'],
          ['load', 'toggleImport'],
          ['export', 'toggleExport'],
          ['config', 'toggleConfig'],
          ['legend', 'toggleLegend'],
          ['logout', 'logout'],
          ['undo', 'undo'],
          ['redo', 'redo']
        ]
    navbarButtons.forEach(([name, msg]) => {
      const el = select('#navbar-' + name)
      el.on('click', null)
      if (this.disabled[name]) {
        addClass(el.node(), 'disabled')
      } else {
        removeClass(el.node(), 'disabled')
        el.on('click', () => this.dispatcher(msg))
      }
    })
  }

  renderCurrency () {
    if (!this.shouldUpdate(true) && !this.shouldUpdate('currency')) return
    this.currencyRoot.innerHTML = currency
    const select = this.currencyRoot.querySelector('select')
    let i = 0
    for (; i < select.options.length; i++) {
      if (select.options[i].value === this.currency) break
    }
    select.options.selectedIndex = i
    select.addEventListener('change', (e) => {
      this.dispatcher('changeCurrency', e.target.value)
    })
  }

  serialize () {
    return this.currency
  }

  deserialize (version, currency) {
    this.currency = currency
  }
}
