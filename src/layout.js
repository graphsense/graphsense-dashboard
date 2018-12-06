import layout from './layout/layout.html'
import Component from './component.js'

export default class Layout extends Component {
  constructor (dispatcher, browser, graph, config, menu, search) {
    super()
    this.dispatcher = dispatcher
    this.browser = browser
    this.graph = graph
    this.config = config
    this.menu = menu
    this.search = search
  }
  triggerFileLoad () {
    this.root.querySelector('#file-loader').click()
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
    if (this.shouldUpdate()) {
      this.root.innerHTML = layout
      this.browser.shouldUpdate(true)
      this.graph.shouldUpdate(true)
      this.config.shouldUpdate(true)
      this.menu.shouldUpdate(true)
      this.search.shouldUpdate(true)
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

        let reader = new FileReader() //eslint-disable-line
        reader.onload = () => {
          let data = reader.result
          this.dispatcher('loadFile', data)
        }
        reader.readAsArrayBuffer(input.files[0])
      })
      browserRoot = this.root.querySelector('#layout-browser')
      graphRoot = this.root.querySelector('#layout-graph')
      configRoot = this.root.querySelector('#layout-config')
      menuRoot = this.root.querySelector('#layout-menu')
      searchRoot = this.root.querySelector('#layout-search')
    }
    this.browser.render(browserRoot)
    this.graph.render(graphRoot)
    this.config.render(configRoot)
    this.menu.render(menuRoot)
    this.search.render(searchRoot)
    super.render()
    return this.root
  }
}
