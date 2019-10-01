import header from './pages/static/boldheader.html'
import footer from './pages/static/footer.hbs'
import Component from './component.js'
import {statsHtml} from './pages/statsHtml.js'

export default class Landingpage extends Component {
  constructor (dispatcher, keyspaces) {
    super()
    this.dispatcher = dispatcher
    this.stats = {}
    this.keyspaces = keyspaces
    keyspaces.forEach(key => {
      this.stats[key] = 'loading'
    })
  }
  setSearch (search) {
    this.search = search
    this.search.setStats(this.stats)
    this.setUpdate(true)
  }
  setLogin (login) {
    this.login = login
  }
  setStats (stats) {
    this.keyspaces.forEach(key => {
      this.stats[key] = stats[key]
    })
    this.setUpdate('stats')
    if (this.search) {
      this.search.setStats(stats)
    }
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate(true)) {
      document.body.style.overflow = 'hidden scroll'
      let stats = statsHtml(this.stats)
      this.root.innerHTML =
        '<div class="' + STATICPAGE_CLASSES + '">' + // eslint-disable-line no-undef
        header + stats + footer({version: VERSION}) + // eslint-disable-line no-undef
        '</div>'
      if (this.search) {
        let searchRoot = this.root.querySelector('.splash .search')
        this.search.setUpdate(true)
        this.search.render(searchRoot)
      } else if (this.login) {
        let loginRoot = this.root.querySelector('.splash .search')
        this.login.setUpdate(true)
        this.login.render(loginRoot)
      }
    } else if (this.shouldUpdate('stats')) {
      this.renderStats()
    } else {
      if (this.search) this.search.render()
      if (this.login) this.login.render()
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
