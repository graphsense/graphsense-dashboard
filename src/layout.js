import layout from './layout/layout.html'

export default class Layout {
  constructor (dispatcher, browser, graph, config) {
    this.dispatcher = dispatcher
    this.root = document.createElement('div')
    this.browser = browser
    this.graph = graph
    this.config = config
  }
  setBrowser (browser) {
    this.browser = browser
    this.renderBrowser()
  }
  setGraph (graph) {
    this.graph = graph
    this.renderGraph()
  }
  setConfig (config) {
    this.config = config
    this.renderConfig()
  }
  renderBrowser () {
    let el = this.root.querySelector('#layout-browser')
    if (el) {
      el.innerHTML = ''
      el.appendChild(this.browser.render())
    }
  }
  renderGraph () {
    let el = this.root.querySelector('#layout-graph')
    if (el) {
      el.innerHTML = ''
      el.appendChild(this.graph.render())
    }
  }
  renderConfig () {
    let el = this.root.querySelector('#layout-config')
    if (el) {
      el.innerHTML = ''
      el.appendChild(this.config.render())
    }
  }
  render () {
    this.root.innerHTML = layout
    this.renderBrowser()
    this.renderGraph()
    this.renderConfig()
    return this.root
  }
}
