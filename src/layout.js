import layout from './layout/layout.html'

export default class Layout {
  constructor (dispatcher, browser) {
    this.dispatcher = dispatcher
    this.root = document.createElement('div')
    this.browser = browser
  }
  setBrowser (browser) {
    this.browser = browser
    this.renderBrowser()
  }
  renderBrowser () {
    let el = this.root.querySelector('#layout-browser')
    if (el) {
      el.innerHTML = ''
      el.appendChild(this.browser.render())
    }
  }
  render () {
    this.root.innerHTML = layout
    this.renderBrowser()
    return this.root
  }
}
