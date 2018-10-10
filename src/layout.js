import layout from './layout/layout.html'

export default class Layout {
  constructor (dispatcher, browser, graph) {
    this.dispatcher = dispatcher
    this.root = document.createElement('div')
    this.browser = browser
    this.graph = graph
  }
  setBrowser (browser) {
    this.browser = browser
    this.renderBrowser()
  }
  setGraph (graph) {
    this.graph = graph
    this.renderGraph()
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
  render () {
    this.root.innerHTML = layout
    this.renderBrowser()
    this.renderGraph()
    return this.root
  }
}
