import layout from './layout/layout.html'
import Component from './component.js'

export default class Layout extends Component {
  constructor (dispatcher, browser, graph, config, search) {
    super()
    this.dispatcher = dispatcher
    this.browser = browser
    this.graph = graph
    this.config = config
    this.search = search
  }
  setBrowser (browser) {
    this.browser = browser
    this.browser.shouldUpdate(true)
  }
  setGraph (graph) {
    this.graph = graph
    this.graph.shouldUpdate(true)
  }
  setConfig (config) {
    this.config = config
    this.config.shouldUpdate(true)
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    // set overflow hidden to prevent window scrollbars
    document.body.style.overflow = 'hidden'
    let browserRoot = null
    let graphRoot = null
    let configRoot = null
    let searchRoot = null
    if (this.shouldUpdate()) {
      this.root.innerHTML = layout
      this.browser.shouldUpdate(true)
      this.graph.shouldUpdate(true)
      this.config.shouldUpdate(true)
      this.search.shouldUpdate(true)
      let saveButton = this.root.querySelector('#navbar-save')
      saveButton.addEventListener('click', () => {
        this.dispatcher('save')
      })
      let loadButton = this.root.querySelector('#navbar-load')
      loadButton.addEventListener('click', () => {
        this.dispatcher('load')
      })
      browserRoot = this.root.querySelector('#layout-browser')
      graphRoot = this.root.querySelector('#layout-graph')
      configRoot = this.root.querySelector('#layout-config')
      searchRoot = this.root.querySelector('#layout-search')
    }
    this.browser.render(browserRoot)
    this.graph.render(graphRoot)
    this.config.render(configRoot)
    this.search.render(searchRoot)
    super.render()
    return this.root
  }
}
