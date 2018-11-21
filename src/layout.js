import layout from './layout/layout.html'
import Component from './component.js'
import {browserHeight} from './globals.js'

export default class Layout extends Component {
  constructor (dispatcher, browser, graph, config) {
    super()
    this.dispatcher = dispatcher
    this.browser = browser
    this.graph = graph
    this.config = config
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
    let browserRoot = null
    let graphRoot = null
    let configRoot = null
    if (this.shouldUpdate()) {
      this.root.innerHTML = layout
      this.browser.shouldUpdate(true)
      this.graph.shouldUpdate(true)
      this.config.shouldUpdate(true)
      browserRoot = this.root.querySelector('#layout-browser')
      graphRoot = this.root.querySelector('#layout-graph')
      configRoot = this.root.querySelector('#layout-config')
      browserRoot.style.height = browserHeight + 'px'
    }
    this.browser.render(browserRoot)
    this.graph.render(graphRoot)
    this.config.render(configRoot)
    super.render()
    return this.root
  }
}
