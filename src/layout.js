import layout from './layout/layout.html'
import Component from './component.js'
import currency from './layout/currency.html'
import {addClass, removeClass} from './template_utils.js'
import {select} from 'd3-selection'

export default class Layout extends Component {
  constructor (dispatcher, browser, graph, config, menu, search, status, login, currency) {
    super()
    this.currency = currency
    this.dispatcher = dispatcher
    this.browser = browser
    this.graph = graph
    this.config = config
    this.menu = menu
    this.search = search
    this.statusbar = status
    this.login = login
    this.currencyRoot = null
    this.disabled = {}
  }
  triggerFileLoad (loadType) {
    this.root.querySelector(`.file-loader[data-type="${loadType}"]`).click()
  }
  setCurrency (currency) {
    this.currency = currency
    this.setUpdate('currency')
  }
  disableButton (name, disable) {
    this.disabled[name] = disable
    this.setUpdate('buttons')
  }
  showLogin (show) {
    this.loginVisible = show
    this.setUpdate('login')
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
    let loginRoot = null
    if (this.shouldUpdate(true)) {
      this.root.innerHTML = layout
      this.browser.setUpdate(true)
      this.graph.setUpdate(true)
      this.config.setUpdate(true)
      this.menu.setUpdate(true)
      this.search.setUpdate(true)
      this.statusbar.setUpdate(true)
      this.login.setUpdate(true)
      this.renderButtons()
      let loaders = this.root.querySelectorAll('.file-loader')
      loaders.forEach(loader => {
        loader.addEventListener('change', (e) => {
          let input = e.target
          let type = e.target.getAttribute('data-type')
          let accept = e.target.getAttribute('accept')

          let reader = new FileReader() // eslint-disable-line no-undef
          let filename = input.files[0].name
          reader.onload = () => {
            let data = reader.result
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
      loginRoot = this.root.querySelector('#layout-login > div')
      this.currencyRoot = this.root.querySelector('#layout-currency-config')
    } else if (this.shouldUpdate('buttons')) {
      this.renderButtons()
    } else if (this.shouldUpdate('login')) {
      this.root.querySelector('#layout-login').style.display = this.loginVisible ? 'flex' : 'none'
    }
    this.browser.render(browserRoot)
    this.graph.render(graphRoot)
    this.config.render(configRoot)
    this.menu.render(menuRoot)
    this.search.render(searchRoot)
    this.statusbar.render(statusRoot)
    this.login.render(loginRoot)
    this.renderCurrency()
    super.render()
    return this.root
  }
  renderButtons () {
    let navbarButtons =
        [ ['new', 'new'],
          ['load', 'toggleImport'],
          ['export', 'toggleExport'],
          ['config', 'toggleConfig'],
          ['legend', 'toggleLegend'],
          ['undo', 'undo'],
          ['redo', 'redo']
        ]
    navbarButtons.forEach(([name, msg]) => {
      let el = select('#navbar-' + name)
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
    let select = this.currencyRoot.querySelector('select')
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
