import layout from './layout/layout.html'
import Component from './component.js'
import currency from './layout/currency.html'

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
  }
  triggerFileLoad () {
    this.root.querySelector('#file-loader').click()
  }
  setCurrency (currency) {
    this.currency = currency
    this.shouldUpdate('currency')
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    // set overflow hidden to prevent window scrollbars
    document.body.style.overflow = 'hidden'
    let browserRoot = null
    let graphRoot = null
    let configRoot = null
    let menuRoot = null
    let searchRoot = null
    let statusRoot = null
    if (this.shouldUpdate() === true) {
      this.root.innerHTML = layout
      this.browser.shouldUpdate(true)
      this.graph.shouldUpdate(true)
      this.config.shouldUpdate(true)
      this.menu.shouldUpdate(true)
      this.search.shouldUpdate(true)
      this.statusbar.shouldUpdate(true)
      let newButton = this.root.querySelector('#navbar-new')
      newButton.addEventListener('click', () => {
        this.dispatcher('new')
      })
      let saveButton = this.root.querySelector('#navbar-save')
      saveButton.addEventListener('click', () => {
        this.dispatcher('save')
      })
      let loadButton = this.root.querySelector('#navbar-load')
      loadButton.addEventListener('click', () => {
        this.dispatcher('load')
      })
      let configButton = this.root.querySelector('#navbar-config')
      configButton.addEventListener('click', () => {
        this.dispatcher('toggleConfig')
      })
      let loader = this.root.querySelector('#file-loader')
      loader.addEventListener('change', (e) => {
        let input = e.target

        let reader = new FileReader() // eslint-disable-line no-undef
        let filename = input.files[0].name
        reader.onload = () => {
          let data = reader.result
          this.dispatcher('loadFile', [data, filename])
        }
        reader.readAsArrayBuffer(input.files[0])
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
  renderCurrency () {
    if (this.shouldUpdate() !== true && this.shouldUpdate() !== 'currency') return
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
  deserialize (currency) {
    this.currency = currency
  }
}
