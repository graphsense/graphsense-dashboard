import header from './pages/static/boldheader.html'
import footer from './pages/static/footer.html'
import Component from './component.js'
import {replace} from './template_utils'
import {statsHtml} from './pages/statsHtml.js'

export default class Landingpage extends Component {
  constructor (dispatcher, search, keyspaces) {
    super()
    this.dispatcher = dispatcher
    this.stats = {}
    this.search = search
    this.keyspaces = keyspaces
    keyspaces.forEach(key => {
      this.stats[key] = 'loading'
    })
  }
  setStats (stats) {
    this.keyspaces.forEach(key => {
      this.stats[key] = stats[key]
    })
    this.setUpdate('stats')
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate(true)) {
      document.body.style.overflow = 'hidden scroll'
      let stats = statsHtml(this.stats)
      this.root.innerHTML =
        '<div class="' + STATICPAGE_CLASSES + '">' + // eslint-disable-line no-undef
        header + stats + replace(footer, {version: VERSION}) + // eslint-disable-line no-undef
        '</div>'
      let searchRoot = this.root.querySelector('.splash .search')
      this.search.setUpdate(true)
      this.search.render(searchRoot)
    } else if (this.shouldUpdate('stats')) {
      this.renderStats()
    } else {
      this.search.render()
    }
    super.render()
    return this.root
  }
  renderStats () {
    let el = document.createElement('div')
    el.innerHTML = statsHtml(this.stats)
    let statsEl = document.querySelector('#stats')
    statsEl.innerHTML = el.firstChild.innerHTML
  }
}
