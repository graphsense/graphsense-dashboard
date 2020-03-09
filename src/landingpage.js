import header from './pages/static/boldheader.html'
import footer from './pages/static/footer.hbs'
import Component from './component.js'
import { statsHtml } from './pages/statsHtml.js'

export default class Landingpage extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
    this.stats = {}
  }

  setSearch (search) {
    this.searchOrLogin = search
    this.searchOrLogin.setStats(this.stats)
    this.showJumpToApp = true
    this.setUpdate(true)
  }

  setLogin (login) {
    if (this.searchOrLogin === login) return
    this.searchOrLogin = login
    this.setUpdate(true)
  }

  setStats (stats) {
    this.stats = stats
    this.setUpdate('stats')
    if (this.searchOrLogin && this.searchOrLogin.setStats) {
      this.searchOrLogin.setStats(stats)
    }
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate(true)) {
      document.body.style.overflow = 'hidden scroll'
      const stats = statsHtml(this.stats)
      this.root.innerHTML =
        '<div class="' + STATICPAGE_CLASSES + '">' + // eslint-disable-line no-undef
        header + stats + footer({ version: VERSION }) + // eslint-disable-line no-undef
        '</div>'
      if (this.searchOrLogin) {
        const searchRoot = this.root.querySelector('.splash .search')
        this.searchOrLogin.setUpdate(true)
        this.searchOrLogin.render(searchRoot)
      }
      if (this.showJumpToApp) {
        const el = this.root.querySelector('#jumpToApp')
        el.innerHTML = '<button class="text-left text-sm text-white" data-msg="jumpToApp">Go to dashboard »</button>'
        el.firstChild.addEventListener('click', () => this.dispatcher('jumpToApp'))
      }
    } else if (this.shouldUpdate('stats')) {
      this.renderStats()
    } else {
      if (this.searchOrLogin) this.searchOrLogin.render()
    }
    super.render()
    return this.root
  }

  renderStats () {
    const el = document.createElement('div')
    el.innerHTML = statsHtml(this.stats)
    const statsEl = document.querySelector('#stats')
    statsEl.innerHTML = el.firstChild.innerHTML
  }
}
